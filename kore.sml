val fb_get_width = _import "fb_get_width" public: unit -> Word32.word;
val fb_get_height = _import "fb_get_height" public: unit -> Word32.word;
val fb_init = _import "fb_init" public: unit -> Int32.int;
val fb_clear = _import "fb_clear" public: Word32.word -> Int32.int;
val fb_fillrect = _import "fb_fillrect" public: Word32.word * Word32.word * Word32.word * Word32.word * Word32.word -> Int32.int;
val fb_wait_vblank = _import "fb_wait_vblank" public: unit -> Int32.int;
val fb_copy = _import "fb_copy" public: unit -> unit;
val input_keyboard_c = _import "input_keyboard" public: unit -> MLton.Pointer.t;
val input_mouse_c = _import "input_mouse" public: unit -> MLton.Pointer.t;
val input_init = _import "input_init" public: unit -> Int32.int;

structure File = struct
  type File = int

  val file_read = fn x =>
    case (_import "file_read" public: int * Word8.word array * int -> int;) x of
      2 => raise Fail "read hit end of file"
    | 1 => raise Fail "read failed"
    | 0 => {}
    | _ => raise Match

  val file_write = fn x =>
    case (_import "file_write" public: int * Word8.word array * int -> int;) x of
      1 => raise Fail "write failed"
    | 0 => {}
    | _ => raise Match

  val op3n = fn x =>
    case (_import "file_open" public: string -> int;) x of
      ~1 => raise Fail "failed to open file"
    | ~2 => raise Fail "failed to read disk signature"
    | ~3 => raise Fail "bad disk signature"
    | x => x

  val seek = fn x =>
    case (_import "file_seek" public: int * Word64.word -> int;) x of
      0 => {}
    | 1 => raise Fail "file_seek fail"
    | _ => raise Match

  val write : int * Word64.word * Word8.word Array.array -> {} = fn (f,pos,a) =>
    (seek (f, pos)
    ;file_write (f, a, Array.length a)
    )

  val read : int * Word64.word * int -> Word8.word Array.array = fn (f,pos,n) =>
    let
      val a = Array.array (n, 0w0)
    in
      (seek (f, pos)
      ;file_read (f, a, n)
      ;a
      )
    end
end :> sig
  type File
  val op3n : string -> File
  val read : File * Word64.word * int -> Word8.word Array.array
  val write : File * Word64.word * Word8.word Array.array -> {}
end

datatype Bit = B0|B1

(* XXX How do you use << from Word8 *)
val decode_word32 : Word8.word Array.array -> Word32.word = fn x =>
  Word32.fromLarge (
    Word8.toLarge (Array.sub (x, 0)) * 0w256*0w256*0w256 +
    Word8.toLarge (Array.sub (x, 1)) * 0w256*0w256 +
    Word8.toLarge (Array.sub (x, 2)) * 0w256 +
    Word8.toLarge (Array.sub (x, 3))
  )

val encode_word32 : Word32.word -> Word8.word Array.array = fn x =>
  Array.fromList
    ([Word8.fromLarge (Word32.toLarge (Word32.andb (x div (0w256*0w256*0w256), 0w255)))
     ,Word8.fromLarge (Word32.toLarge (Word32.andb (x div (0w256*0w256), 0w255)))
     ,Word8.fromLarge (Word32.toLarge (Word32.andb (x div 0w256, 0w255)))
     ,Word8.fromLarge (Word32.toLarge (Word32.andb (x, 0w255)))
     ])

val rec shift_left : Word8.word * int -> Word8.word = fn
    (w,0) => w
  | (w,n) => shift_left (w*0w2,n-1)

val rec pack_bits_ : Bit list * int * Word8.word -> Word8.word list = fn
    (bs,8,w)    => w::pack_bits_ (bs,0,0w0)
  | ([],0,w)    => []
  | ([],_,w)    => [w]
  | (b::bs,c,w) => pack_bits_ (bs, c+1, (Word8.orb (w, shift_left (if b=B0 then 0w0 else 0w1, c))))

val pack_bits : Bit list -> Word8.word Array.array = fn bs =>
  Array.fromList (pack_bits_ (bs,0,0w0))

val rec unpack_word : Word8.word * Word32.word -> Bit list = fn
    (_,0w0) => []
  | (w,n) =>
      (case Word8.andb (w,0w1) of 0w0 => B0 | _ => B1) :: unpack_word (w div 0w2, n-0w1)

val rec unpack_bits_ : Word8.word Array.array * int * Word32.word -> Bit list = fn
  (_,_,0w0) => []
| (a,i,len) =>
    let val x = Word32.min (len, 0w8)
    in unpack_word (Array.sub (a,i), x) @ unpack_bits_ (a,i+1,len-x)
    end

val unpack_bits : Word8.word Array.array * Word32.word -> Bit list = fn (a, len) =>
  unpack_bits_ (a, 0, len)

val w32to64 : Word32.word -> Word64.word = Word64.fromLarge o Word32.toLarge
val w64to32 : Word64.word -> Word32.word = Word32.fromLarge o Word64.toLarge

structure OnDiskKVStore = struct
  val op3n = fn p =>
    let val f = File.op3n p
    in
      if case File.read (f, 0w0, 4) of x => List.map (fn i => Array.sub (x, i)) [0,1,2,3] = [0w33,0w123,0w234,0w72]
      then f
      else raise Fail "bad signature for OnDiskKVStore"
    end

  val uh = fn (xs,ys,pfx) => (xs,ys,List.rev pfx)
  val rec splice' : Bit list * Bit list * Bit list -> Bit list * Bit list * Bit list = fn
      a as(x::xs, y::ys, pfx) => if x=y then splice' (xs,ys,x::pfx) else uh a
    | a => uh a
  val splice : Bit list * Bit list -> Bit list * Bit list * Bit list = fn (xs,ys)
    => splice' (xs,ys,[])

  val bslen : Word32.word -> Word32.word = fn x =>
    x div 0w8 + (if x mod 0w8 = 0w0 then 0w0 else 0w1)

  val put_value : {file: File.File
                  ,value: Word8.word list
                  ,free: Word64.word
                  } -> Word64.word = fn x =>
    (File.write (#file x, #free x, Array.fromList (#value x))
    ;#free x + Word64.fromInt (List.length (#value x))
    )
  val put_node : {file: File.File
                 ,key: Bit list
                 ,free: Word64.word
                 ,b0: Word32.word
                 ,b1: Word32.word
                 ,dsize: Word32.word 
                 ,dpos: Word32.word 
                 } -> Word64.word = fn arg =>
    let
      val bl = pack_bits (#key arg)
      val y = #free arg + 0w4 + Word64.fromInt (Array.length bl)
    in
      (File.write (#file arg, #free arg, (encode_word32 o Word32.fromInt o List.length o #key) arg)
      ;File.write (#file arg, #free arg + 0w4, bl)
      ;File.write (#file arg, y + 0w0, encode_word32 (#b0 arg))
      ;File.write (#file arg, y + 0w4, encode_word32 (#b1 arg))
      ;File.write (#file arg, y + 0w8, encode_word32 (#dsize arg))
      ;File.write (#file arg, y + 0w12, encode_word32 (#dpos arg))
      ;y+ 0w16
      )
    end

  val rec put_ : {file: File.File 
                 ,key: Bit list
                 ,value: Word8.word list
                 ,pos: Word64.word
                 ,free: Word64.word
                 } -> Word64.word = fn arg =>
    let
      val kl = decode_word32 (File.read (#file arg, #pos arg, 4))
      val cl = bslen kl
      val bl = unpack_bits (File.read (#file arg, #pos arg+0w4, Word32.toInt cl), kl)
      val abc = #pos arg + 0w4 + w32to64 cl
      val b0 = decode_word32 (File.read (#file arg, abc, 4))
      val b1 = decode_word32 (File.read (#file arg, abc+0w4, 4))
      val dsize = decode_word32 (File.read (#file arg, abc+0w8, 4))
      val dpos = decode_word32 (File.read (#file arg, abc+0w12, 4))
      val (xs,ys,pfx) = splice (#key arg,bl)
      val v_len = fn _ => Word32.fromInt (List.length (#value arg))
    in
      case (xs,ys) of
        ([],[]) =>
          let
            val free' = put_value {file= #file arg, value= #value arg, free= #free arg}
          in
            (File.write (#file arg, abc + 0w8, encode_word32 (v_len {}))
            ;File.write (#file arg, abc + 0w12, encode_word32 (w64to32 (#free arg)))
            ;free'
            )
          end
      | (x::xs,[]) =>
          if (case x of B0 => b0 | B1 => b1) = 0w0
          then
            let
              val free' = put_value {file= #file arg, value= #value arg, free= #free arg}
              val free'' = put_node
                            {file= #file arg
                            ,key=xs
                            ,free=free'
                            ,b0=0w0
                            ,b1=0w0
                            ,dsize=v_len {}
                            ,dpos= w64to32 (#free arg)}
            in
              (File.write (#file arg, abc + (case x of B0 => 0w0 | B1 => 0w4), encode_word32 (w64to32 free'))
              ;free''
              )
            end
          else
            put_ {file= #file arg
                 ,key=xs
                 ,free= #free arg
                 ,pos=w32to64 (case x of B0 => b0 | B1 => b1)
                 ,value= #value arg
                 }
      | (_,y::ys) =>
          let
            val free' = put_value {file= #file arg, value= #value arg, free= #free arg}
            val (free'', newbranch) =
              case xs of
                 x::xs =>
                   (put_node
                      {file= #file arg
                      ,key=xs
                      ,free=free'
                      ,b0=0w0
                      ,b1=0w0
                      ,dsize=v_len {}
                      ,dpos=w64to32 (#free arg)
                      }
                   ,free'
                   )
               | _ => (free', 0w0)
            val free''' = put_node
                            {file= #file arg
                            ,key=ys
                            ,free=free''
                            ,b0=b0
                            ,b1=b1
                            ,dsize=dsize
                            ,dpos=dpos
                            }
          in
            (put_node {file= #file arg
                      ,key=pfx
                      ,free= #pos arg
                      ,b0=w64to32 (case y of B0 => free'' | B1 => newbranch)
                      ,b1=w64to32 (case y of B1 => free'' | B0 => newbranch)
                      ,dsize=(case xs of [] => v_len {} | _ => 0w0)
                      ,dpos=(case xs of [] => w64to32 (#free arg) | _ => 0w0)
                      }
            ;free'''
            )
          end
    end

  val put : {file: File.File
            ,key: Bit list
            ,value: Word8.word list
            } -> {} = fn x =>
    case put_
           {file= #file x 
           ,key= #key x
           ,value = #value x
           ,pos = 0w8
           ,free = w32to64 (decode_word32 (File.read(#file x, 0w4, 4)))}
    of free' => File.write (#file x, 0w4, encode_word32 (w64to32 free'))
      

  val rec j = fn
      ([], _::_) => NONE
    | (k::ks, b::bs) => if k=b then j (ks, bs) else NONE
    | (k, []) => SOME k

  val rec get_ : {file: File.File, pos: Word64.word, key: Bit list} -> (Word8.word Array.array) option = fn arg =>
    let
      val kl = decode_word32 (File.read (#file arg, #pos arg, 4))
      val cl = bslen kl
      val bl = unpack_bits (File.read (#file arg, #pos arg+0w4, Word32.toInt cl), kl)
      val abc = #pos arg + 0w4 + w32to64 cl
      val b0 = decode_word32 (File.read (#file arg, abc, 4))
      val b1 = decode_word32 (File.read (#file arg, abc+0w4, 4))
      val dsize = decode_word32 (File.read (#file arg, abc+0w8, 4))
      val dpos = decode_word32 (File.read (#file arg, abc+0w12, 4))
      val (xs,ys,pfx) = splice (#key arg,bl)
    in
      case j (#key arg, bl) of
         NONE => NONE
       | SOME [] =>
           (case dpos of
              0w0 => NONE
            | _ => SOME (File.read (#file arg, w32to64 dpos, Word32.toInt dsize)))
       | SOME (k::ks) =>
          (case (case k of B0 => b0 | B1 => b1) of
             0w0 => NONE
           | branch =>
               get_ {file= #file arg, pos=w32to64 branch, key=ks}
          )
    end
  val get : {file: File.File, key: Bit list} -> (Word8.word Array.array) option =
    fn x => get_ {file= #file x, pos=0w8, key= #key x}
end :> sig
  val op3n : string -> File.File
  val get : {file: File.File, key: Bit list} -> (Word8.word Array.array) option
  val put : {file: File.File
            ,key: Bit list
            ,value: Word8.word list
            } -> {}
end

val id = fn a => a

structure List2 = struct
  val rec modifyAt : int * ('a -> 'a) * 'a list -> ('a list) option = fn
    (_, _, []     ) => NONE
  | (0, f, (x::xs)) => SOME(f x :: xs)
  | (i, f, (x::xs)) =>
    case modifyAt(i-1, f, xs) of
      NONE => NONE
    | SOME xs => SOME(x::xs)

  val rec insertAt : int * 'a * 'a list -> ('a list) option = fn
    (0, e,    xs) => SOME(e::xs)
  | (i, _, []   ) => NONE
  | (i, e, x::xs) =>
    case insertAt(i-1, e, xs) of
      NONE    => NONE
    | SOME xs => SOME(x::xs)

  val replaceAt : int * 'a * 'a list -> ('a list) option = fn
    (i, e, xs) => modifyAt(i, fn _ => e, xs)

  val rec secondLast : 'a list -> 'a option = fn
    [] => NONE
  | (_::[]) => NONE
  | (x::_::[]) => SOME x
  | (_::xs) => secondLast xs

  local
    val rec index' = fn
      (_, []) => []
    | (i, x::xs) => (i, x) :: index' (i+1, xs)
  in
    val index : 'a list -> (int * 'a) list = fn xs => index'(0, xs)
  end
end

structure Option2 = struct
  exception NotSOME
  val unSOME = fn
    NONE => raise NotSOME
  | SOME(x) => x
end

val w32i = Word32.fromInt

functor Kore(T : sig type id end) = struct

  datatype Code = CProduct of (T.id * Code) list
                | CUnion of (T.id * Code) list

  datatype ICode = ICProduct of (T.id * ICode) list
                 | ICUnion of (T.id * ICode) list
                 | IC_PU of (T.id * ICode) list

  datatype Relation = RRef of T.id
                    | RCode of Code 
                    | RConstruct of T.id
                    | RProduct of (T.id * Relation) list
                    | RProjection of T.id
                    | RUnion of Relation list
                    | RComposition of Relation list

  datatype TypedRelation = TRRef of ICode * T.id * ICode
                         | TRCode of ICode * Code * ICode
                         | TRConstruct of ICode * T.id * ICode
                         | TRProduct of ICode * (T.id * TypedRelation) list * ICode
                         | TRProjection of ICode * T.id * ICode
                         | TRUnion of ICode * TypedRelation list * ICode
                         | TRComposition of ICode * TypedRelation list * ICode

  datatype Value = VLabel of T.id * Value | VProduct of (T.id * Value) list

  val typeRelation : Relation -> TypedRelation = fn _ => TRUnion ((IC_PU []), [], (IC_PU []))

  val eval : Value * Relation -> Value = fn _ => VProduct []

  datatype RelationPathElem = RPEIndex of int
                            | RPEId
                            | RPEProductLabel | RPEProductRel

  structure RN = struct
    datatype T = Rel of Relation | LabelId of T.id | RelId of T.id
    val unRel : T -> Relation = fn Rel r => r
    val unRelNF : Relation * T -> Relation = fn
      (_, Rel r) => r
    | (d, _) => d
    val unLabelId : T -> T.id = fn LabelId i => i
    val unLabelIdNF : T.id * T -> T.id = fn
      (_, LabelId i) => i
    | (d, _) => d
    val unRelId : T -> T.id = fn RelId i => i
    val unRelIdNF : T.id * T -> T.id = fn
      (_, RelId i) => i
    | (d, _) => d
  end

  val rec modifyRelationAt : RelationPathElem list * (RN.T -> RN.T) * Relation -> Relation = fn
    ([], f, root) => RN.unRelNF (root, f (RN.Rel root))
  | ([RPEId], f, RProjection i) => RProjection (RN.unLabelIdNF (i, f (RN.LabelId i)))
  | ([RPEId], f, RConstruct  i) => RConstruct  (RN.unLabelIdNF (i, f (RN.LabelId i)))
  | ([RPEId], f, RRef        i) => RRef        (RN.unRelIdNF   (i, f (RN.RelId   i)))
  | (RPEIndex i :: path, f, RUnion rs) =>
      RUnion(
        Option2.unSOME(
          List2.modifyAt(
            i,
            fn root' => modifyRelationAt(path, f, root'),
            rs)))
  | (RPEIndex i :: path, f, RComposition rs) =>
      RComposition(
        Option2.unSOME(
          List2.modifyAt(
            i,
            fn root' => modifyRelationAt(path, f, root'),
            rs)))
  | (RPEIndex i :: path, f, RProduct rs) => 
      RProduct(
        Option2.unSOME(
          List2.modifyAt(
            i,
            fn c => modifyProdCell(path, f, c),
            rs)))
  | _ => raise Match
  and modifyProdCell = fn
    ([RPEProductLabel], f, (id, r)) => (RN.unLabelIdNF (id, f (RN.LabelId id)), r)
  | (RPEProductRel :: path, f, (id, r)) => (id, modifyRelationAt(path, f, r))

  val replaceRelationAt : RelationPathElem list * RN.T * Relation -> Relation = fn
    (path, r, root) => modifyRelationAt(path, fn _ => r, root)

  val relationAt : RelationPathElem list * Relation -> RN.T = fn (path, root) =>
    let
      val r = ref NONE
    in
      modifyRelationAt (path, fn x => (r := SOME x; x), root);
      Option2.unSOME (!r)
    end

  val parentRelation : RelationPathElem list * Relation -> Relation option = fn (path, root) =>
    case rev path of
      [] => NONE
    | xs =>
        let
          val p = case xs of
            (RPEId :: xs) => xs
          | (RPEProductRel :: RPEIndex _ :: xs) => xs
          | (RPEProductLabel :: RPEIndex _ :: xs) => xs
          | (RPEIndex _ :: xs) => xs
          | _ => raise Match
        in
          (SOME o RN.unRel o relationAt) (rev p, root)
        end
end

structure K2 = Kore(struct type id = Bit list end)
structure K1 = Kore(struct type id = string end)

val printValue : K1.Value -> string = fn _ => ""

(*
Coordinate System

There is a 2 dimensional space of infinite size. We display "objects" within it.
The computer monitor displays a small area within of this space. We call this area the viewport.
The viewport can be moved around (and resized if you change the monitor resolution).

y at the top of the space is 0 and H (height) - 1 at the bottom
x at the left of the space is 0 and W (width) - 1 at the rightmost position
0 |
1 |
2 |
3 |
4 +-----------
   1 2 3 4

Panning is done using two variables `panh` (horizontal) and `panv` (vertical), expressing how far panned the Object Under Display is in each dimension.
So if `panh` is a positive number, the object would be right of the _start_ of the viewport:

left side of viewport
|
|  
|  
|
|    o
|

And if `panh` was negative, the object would be left of the start of viewport:

      left side of viewport
      |
      |
      |
      |
 o    |
      |

And if `panv` was positive, the object would be under the viewport:

-------------- top of viewport

      o

And if `panv` was negative, the object would be above the viewport:

      o
      
-------------- top of viewport
*)

(* TODO use records *)
datatype Box = Box of int * int * (((int*int*int*int*int) -> unit) -> unit) * (int * int * Box) list

val panh = ref 0
val panv = ref 0

val makeBoundFillrectFun : int*int*int*int -> int*int*int*int*int -> unit =
  fn (bx,by,bw,bh) =>
    fn (x,y,w,h,v) => case x<bw andalso y<bh of
      true => (fb_fillrect(w32i(bx+x),w32i(by+y),w32i w,w32i h,w32i v);())
    | false => raise Match

val rec renderBox = fn (bx:int,by:int,Box(bw,bh,renderFun,boxes)) =>
  case (w32i bx < fb_get_width() andalso w32i by < fb_get_height() andalso bx+bw > 0 andalso by+bh > 0) of
    true => (renderFun(makeBoundFillrectFun(bx,by,bw,bh))
            ;map
              (fn (cx,cy,cb) =>
                (case cx>=0 andalso cy>=0 of true =>()
                ;renderBox(bx+cx,by+cy,cb)))
              boxes
            ;())
  | false => ()

val borderWidth = 10

val backgroundColor = 0x00aa00
val unionColor = 0xaa0000
val productColor = 0xaaaa00
val compositionColor = 0xaa00aa
val unionInnerColor = 0xd00000
val productInnerColor = 0xdddd00
val compositionInnerColor = 0xdd00dd
val refColor = 0xaaaaaa
val constructColor = productColor
val projectionColor = 0x00ffaa
val edgeColor = 0
val selectColor = 0xbb0099
val edgeWidth = 1

(*
Ideas for visually representing long bit strings (like 128-bit or 256-bit)
--------
System 1:
--------
128-bit
a 4 by 4 box of of cells each with 1 of 16 colors and 1 of 16 shapes
alternate (Tim's idea): put boxes within each box, each of a different color (of 16)

Tim's ideas: /"orderings (factorial growth) have faster growth than exponentiation"/
--------
System 2:
--------
256-bit
+--------------------+ (short example)
|c7|c5|c4|c2|c3|c6|c1|
+--------------------+
A box of 58 colors, sorted by the identifier number
Number of combinations is 58!

--------
System 3:
--------
256-bit
4 boxes of 21 colors
+--------------------+ (short example)
|c7|c5|c4|c2|c3|c6|c1|
+--------------------+
+--------------------+
|c5|c7|c2|c3|c6|c4|c1|
+--------------------+
+--------------------+
|c5|c4|c1|c3|c2|c6|c7|
+--------------------+
+--------------------+
|c3|c2|c4|c5|c1|c7|c6|
+--------------------+
21^4


k rows, ceil(58/k) columns

--------
System 4
--------
256-bit
16 colors
8 Columns followed by a 14*4 grid (64 cells total)
Idea is that the first 8 columns already are unprobable to be not unique within any set you care about
| | | | | | | | |-|-|-|-|-|
| | | | | | | | |-|-|-|-|-|
| | | | | | | | |-|-|-|-|-|
| | | | | | | | |-|-|-|-|-|
| | | | | | | | |-|-|-|-|-|
alt:
4 bars and 15x4 grid
first part will have 65K combinations

can represent slightly more than all 256-bit numbers:
32 colors
4 columns followed by a 12x4 grid
over 1 million combinations in first 4 columns
could also use 4 shapes and 8 colors

Using orders can increase complexity and reduce the ease of creating "vanity identifiers"

--------
System 5
--------
WF's idea: representation based on set-theoretic definition of natural numbers
like "von Neumann ordinalls":
  0 = {}
  n + 1 = n U {n}
example:
  0: {}
  1: {{}}
  2: {{}, {{}}}
  3: {{}, {{}}, {{}, {{}}}}
...but encoding the 128-bit number straight to a von Neumann ordinal wouldn't work, it would use exponential space.
so im not sure what the actual idea was. maybe something like:
for 2 bit number:
  0 = {{}, {{}}}
  1 = {{{}}, {}}
for 4 bit number:
  {a, b, c}
  {b, a, c}
  {c, a, b}
  {c, b, a}
  0 = {{}, {{}}, {{}, {{}}}}
  1 = {{{}}, {}, {{}, {{}}}}
  2 = {{{}, {{}}}, {{}}, {}}
  3 = {{{}, {{}}}, {}, {{}}}
which is just permutation, but we can also permute the inner sets:
  0 = {{}, {{}}, {{}, {{}}}}
 +4 = {{}, {{}}, {{{}}, {}}}
  1 = {{{}}, {}, {{}, {{}}}}
 +5 = {{{}}, {}, {{{}}, {}}}
  2 = {{{}, {{}}}, {{}}, {}}
 +6 = {{{{}}, {}}, {{}}, {}}
  3 = {{{}, {{}}}, {}, {{}}}
 +7 = {{{{}}, {}}, {}, {{}}}

*)

val idCellWidth=8
val idColors =
  [0xaaaa00
  ,0x00aa00
  ,0x770000
  ,0x550000
  ,0x007700
  ,0x005500
  ,0x777700
  ,0x775500
  ,0x555500
  ,0x770055
  ,0x550055
  ,0x882288
  ,0x007755
  ,0x003388
  ,0x559999
  ,0x777755
  ]
val idShapes =
[[[0,0,0,0,0,0,0,0]
 ,[0,1,1,1,1,1,1,0]
 ,[0,1,1,1,1,1,1,0]
 ,[0,1,1,0,0,0,0,0]
 ,[0,1,1,0,0,0,0,0]
 ,[0,1,1,1,1,1,1,0]
 ,[0,1,1,1,1,1,1,0]
 ,[0,0,0,0,0,0,0,0]]
,[[0,0,0,0,0,0,0,0]
 ,[0,1,1,0,0,1,1,0]
 ,[0,1,1,0,0,1,1,0]
 ,[0,1,1,0,0,1,1,0]
 ,[0,1,1,0,0,1,1,0]
 ,[0,1,1,1,1,1,1,0]
 ,[0,1,1,1,1,1,1,0]
 ,[0,0,0,0,0,0,0,0]]
,[[0,0,0,0,0,0,0,0]
 ,[0,1,1,1,1,1,1,0]
 ,[0,1,1,1,1,1,1,0]
 ,[0,1,1,0,0,1,1,0]
 ,[0,1,1,0,0,1,1,0]
 ,[0,1,1,0,0,1,1,0]
 ,[0,1,1,0,0,1,1,0]
 ,[0,0,0,0,0,0,0,0]]
,[[0,0,0,0,0,0,0,0]
 ,[0,1,1,1,1,1,1,0]
 ,[0,1,1,1,1,1,1,0]
 ,[0,0,0,0,0,1,1,0]
 ,[0,0,0,0,0,1,1,0]
 ,[0,1,1,1,1,1,1,0]
 ,[0,1,1,1,1,1,1,0]
 ,[0,0,0,0,0,0,0,0]]
,[[0,0,0,0,0,0,0,0]
 ,[0,1,1,1,1,1,1,0]
 ,[0,1,1,1,1,1,1,0]
 ,[0,1,1,0,0,1,1,0]
 ,[0,1,1,0,0,1,1,0]
 ,[0,1,1,1,1,1,1,0]
 ,[0,1,1,1,1,1,1,0]
 ,[0,0,0,0,0,0,0,0]]
,[[0,0,0,0,0,0,0,0]
 ,[0,1,1,0,0,1,1,0]
 ,[0,1,1,0,0,1,1,0]
 ,[0,1,1,0,0,1,1,0]
 ,[0,1,1,0,0,1,1,0]
 ,[0,1,1,1,1,1,1,0]
 ,[0,1,1,1,1,1,1,0]
 ,[0,0,0,0,0,0,0,0]]
,[[0,0,0,0,0,0,0,0]
 ,[0,0,0,1,1,0,0,0]
 ,[0,0,1,1,1,1,0,0]
 ,[0,1,0,0,0,0,1,0]
 ,[0,1,1,0,0,1,1,0]
 ,[0,0,1,1,1,1,0,0]
 ,[0,0,0,1,1,0,0,0]
 ,[0,0,0,0,0,0,0,0]]
,[[0,0,0,0,0,0,0,0]
 ,[0,0,0,1,1,0,0,0]
 ,[0,0,1,1,1,1,0,0]
 ,[0,1,1,0,0,1,1,0]
 ,[0,1,0,0,0,0,1,0]
 ,[0,0,0,0,0,0,0,0]
 ,[0,0,0,0,0,0,0,0]
 ,[0,0,0,0,0,0,0,0]]
,[[0,0,0,0,0,0,0,0]
 ,[0,0,0,0,0,0,0,0]
 ,[0,0,0,0,0,0,0,0]
 ,[0,1,0,0,0,0,1,0]
 ,[0,1,1,0,0,1,1,0]
 ,[0,0,1,1,1,1,0,0]
 ,[0,0,0,1,1,0,0,0]
 ,[0,0,0,0,0,0,0,0]]
,[[0,0,0,0,0,0,0,0]
 ,[0,0,0,0,0,0,0,0]
 ,[0,0,1,1,1,1,0,0]
 ,[0,0,1,1,1,1,0,0]
 ,[0,0,1,1,1,1,0,0]
 ,[0,0,1,1,1,1,0,0]
 ,[0,0,0,0,0,0,0,0]
 ,[0,0,0,0,0,0,0,0]]
,[[0,0,0,0,0,0,0,0]
 ,[0,0,0,0,0,0,0,0]
 ,[0,0,1,1,1,1,0,0]
 ,[0,0,1,0,0,1,0,0]
 ,[0,0,1,0,0,1,0,0]
 ,[0,0,1,1,1,1,0,0]
 ,[0,0,0,0,0,0,0,0]
 ,[0,0,0,0,0,0,0,0]]
,[[0,0,0,0,0,0,0,0]
 ,[0,0,1,0,0,1,0,0]
 ,[0,1,1,1,1,1,1,0]
 ,[0,0,1,0,0,1,0,0]
 ,[0,0,1,0,0,1,0,0]
 ,[0,1,1,1,1,1,1,0]
 ,[0,0,1,0,0,1,0,0]
 ,[0,0,0,0,0,0,0,0]]
,[[0,0,0,0,0,0,0,0]
 ,[0,0,0,1,1,0,0,0]
 ,[0,0,0,1,1,0,0,0]
 ,[0,1,1,1,1,1,1,0]
 ,[0,1,1,1,1,1,1,0]
 ,[0,0,0,1,1,0,0,0]
 ,[0,0,0,1,1,0,0,0]
 ,[0,0,0,0,0,0,0,0]]
,[[0,0,0,0,0,0,0,0]
 ,[0,1,1,1,1,1,1,0]
 ,[0,1,1,1,1,1,1,0]
 ,[0,0,0,1,1,0,0,0]
 ,[0,0,0,1,1,0,0,0]
 ,[0,0,0,1,1,0,0,0]
 ,[0,0,0,1,1,0,0,0]
 ,[0,0,0,0,0,0,0,0]]
,[[0,0,0,0,0,0,0,0]
 ,[0,0,0,1,1,0,0,0]
 ,[0,0,0,1,1,0,0,0]
 ,[0,0,0,1,1,0,0,0]
 ,[0,0,0,1,1,0,0,0]
 ,[0,1,1,1,1,1,1,0]
 ,[0,1,1,1,1,1,1,0]
 ,[0,0,0,0,0,0,0,0]]
,[[0,0,0,0,0,0,0,0]
 ,[0,1,1,1,1,1,1,0]
 ,[0,1,1,1,1,1,1,0]
 ,[0,0,0,1,1,0,0,0]
 ,[0,0,0,1,1,0,0,0]
 ,[0,1,1,1,1,1,1,0]
 ,[0,1,1,1,1,1,1,0]
 ,[0,0,0,0,0,0,0,0]]
,[[0,0,0,0,0,0,0,0]
 ,[0,1,1,1,1,0,0,0]
 ,[0,1,1,1,1,0,0,0]
 ,[0,0,0,1,1,0,0,0]
 ,[0,0,0,1,1,0,0,0]
 ,[0,1,1,1,1,1,1,0]
 ,[0,1,1,1,1,1,1,0]
 ,[0,0,0,0,0,0,0,0]]
,[[0,0,0,0,0,0,0,0]
 ,[0,1,1,0,0,0,0,0]
 ,[0,1,1,0,0,0,0,0]
 ,[0,1,1,0,0,0,0,0]
 ,[0,1,1,0,0,0,0,0]
 ,[0,1,1,1,1,1,1,0]
 ,[0,1,1,1,1,1,1,0]
 ,[0,0,0,0,0,0,0,0]]
,[[0,0,0,0,0,0,0,0]
 ,[0,0,0,0,0,1,1,0]
 ,[0,0,0,0,0,1,1,0]
 ,[0,0,0,0,0,1,1,0]
 ,[0,0,0,0,0,1,1,0]
 ,[0,1,1,1,1,1,1,0]
 ,[0,1,1,1,1,1,1,0]
 ,[0,0,0,0,0,0,0,0]]
,[[0,0,0,0,0,0,0,0]
 ,[0,1,1,1,1,1,1,0]
 ,[0,1,1,1,1,1,1,0]
 ,[0,0,0,0,0,1,1,0]
 ,[0,0,0,0,0,1,1,0]
 ,[0,0,0,0,0,1,1,0]
 ,[0,0,0,0,0,1,1,0]
 ,[0,0,0,0,0,0,0,0]]
,[[0,0,0,0,0,0,0,0]
 ,[0,1,1,1,1,1,1,0]
 ,[0,1,1,1,1,1,1,0]
 ,[0,1,1,0,0,0,0,0]
 ,[0,1,1,0,0,0,0,0]
 ,[0,1,1,0,0,0,0,0]
 ,[0,1,1,0,0,0,0,0]
 ,[0,0,0,0,0,0,0,0]]
,[[0,0,0,0,0,0,0,0]
 ,[0,0,0,1,1,1,1,0]
 ,[0,0,0,1,1,1,1,0]
 ,[0,0,0,1,1,0,0,0]
 ,[0,0,0,1,1,0,0,0]
 ,[0,1,1,1,1,1,1,0]
 ,[0,1,1,1,1,1,1,0]
 ,[0,0,0,0,0,0,0,0]]
,[[0,0,0,0,0,0,0,0]
 ,[0,1,1,0,0,1,1,0]
 ,[0,1,1,0,0,1,1,0]
 ,[0,1,1,1,1,1,1,0]
 ,[0,1,1,1,1,1,1,0]
 ,[0,0,0,0,0,1,1,0]
 ,[0,0,0,0,0,1,1,0]
 ,[0,0,0,0,0,0,0,0]]
,[[0,0,0,0,0,0,0,0]
 ,[0,1,1,1,1,1,1,0]
 ,[0,1,1,1,1,1,1,0]
 ,[0,0,0,1,1,0,0,0]
 ,[0,0,0,1,1,0,0,0]
 ,[0,1,1,1,1,0,0,0]
 ,[0,1,1,1,1,0,0,0]
 ,[0,0,0,0,0,0,0,0]]
,[[0,0,0,0,0,0,0,0]
 ,[0,1,1,0,0,1,1,0]
 ,[0,1,1,0,0,1,1,0]
 ,[0,1,1,1,1,1,1,0]
 ,[0,1,1,1,1,1,1,0]
 ,[0,1,1,0,0,0,0,0]
 ,[0,1,1,0,0,0,0,0]
 ,[0,0,0,0,0,0,0,0]]
,[[0,0,0,0,0,0,0,0]
 ,[0,1,1,0,0,0,0,0]
 ,[0,1,1,0,0,0,0,0]
 ,[0,1,1,1,1,1,1,0]
 ,[0,1,1,1,1,1,1,0]
 ,[0,1,1,0,0,1,1,0]
 ,[0,1,1,0,0,1,1,0]
 ,[0,0,0,0,0,0,0,0]]
,[[0,0,0,0,0,0,0,0]
 ,[0,1,1,0,0,0,0,0]
 ,[0,1,1,0,0,0,0,0]
 ,[0,1,1,1,1,0,0,0]
 ,[0,1,1,1,1,0,0,0]
 ,[0,1,1,1,1,1,1,0]
 ,[0,1,1,1,1,1,1,0]
 ,[0,0,0,0,0,0,0,0]]
,[[0,0,0,0,0,0,0,0]
 ,[0,1,1,1,1,1,1,0]
 ,[0,1,1,1,1,1,1,0]
 ,[0,0,0,1,1,1,1,0]
 ,[0,0,0,1,1,1,1,0]
 ,[0,0,0,0,0,1,1,0]
 ,[0,0,0,0,0,1,1,0]
 ,[0,0,0,0,0,0,0,0]]
,[[0,0,0,0,0,0,0,0]
 ,[0,1,1,1,1,1,1,0]
 ,[0,1,1,1,1,1,1,0]
 ,[0,1,1,1,1,0,0,0]
 ,[0,1,1,1,1,0,0,0]
 ,[0,1,1,0,0,0,0,0]
 ,[0,1,1,0,0,0,0,0]
 ,[0,0,0,0,0,0,0,0]]
,[[0,0,0,0,0,0,0,0]
 ,[0,0,0,0,0,1,1,0]
 ,[0,0,0,0,0,1,1,0]
 ,[0,0,0,1,1,1,1,0]
 ,[0,0,0,1,1,1,1,0]
 ,[0,1,1,1,1,1,1,0]
 ,[0,1,1,1,1,1,1,0]
 ,[0,0,0,0,0,0,0,0]]
,[[1,1,1,0,0,1,1,1]
 ,[1,1,0,0,0,0,1,1]
 ,[1,0,0,0,0,0,0,1]
 ,[0,0,0,0,0,0,0,0]
 ,[0,0,0,0,0,0,0,0]
 ,[1,0,0,0,0,0,0,1]
 ,[1,1,0,0,0,0,1,1]
 ,[1,1,1,0,0,1,1,1]]
,[[1,1,0,0,0,0,1,1]
 ,[1,0,0,0,0,0,0,1]
 ,[0,0,0,0,0,0,0,0]
 ,[0,0,0,0,0,0,0,0]
 ,[0,0,0,0,0,0,0,0]
 ,[0,0,0,0,0,0,0,0]
 ,[1,0,0,0,0,0,0,1]
 ,[1,1,0,0,0,0,1,1]]
,[[0,0,1,1,1,1,0,0]
 ,[0,0,0,1,1,0,0,0]
 ,[1,0,0,0,0,0,0,1]
 ,[1,1,0,0,0,0,1,1]
 ,[1,1,0,0,0,0,1,1]
 ,[1,0,0,0,0,0,0,1]
 ,[0,0,0,1,1,0,0,0]
 ,[0,0,1,1,1,1,0,0]]
,[[1,1,0,0,0,0,1,1]
 ,[1,1,0,0,0,0,1,1]
 ,[0,1,1,0,0,1,1,0]
 ,[0,0,0,1,1,0,0,0]
 ,[0,0,0,1,1,0,0,0]
 ,[0,1,1,0,0,1,1,0]
 ,[1,1,0,0,0,1,1,1]
 ,[1,1,0,0,0,0,1,1]]
,[[0,0,0,1,1,0,0,0]
 ,[0,0,0,1,1,0,0,0]
 ,[0,0,0,1,1,0,0,0]
 ,[1,1,1,1,1,1,1,1]
 ,[1,1,1,1,1,1,1,1]
 ,[0,0,0,1,1,0,0,0]
 ,[0,0,0,1,1,0,0,0]
 ,[0,0,0,1,1,0,0,0]]
]
val rec drawIdShape = fn
  (_,64) => ()
 |(a as (x,y,fillrect,shape),n) => (
    let val xi = n mod idCellWidth
        val yi = n div idCellWidth
    in
      case List.nth(List.nth(shape,yi),xi) of
        0 => ()
      | 1 => fillrect(x+xi,y+yi,1,1,0)
      | _ => raise Match
    end;
    drawIdShape(a,n+1)
 )
  
val rec splitAt' = fn(xs, 0, ys) => (rev ys, xs)
                   | (x::xs, n, ys) => splitAt'(xs, n-1, x::ys)
                   | _ => raise Fail "splitAt invalid argument"
val splitAt = fn(x, n) => splitAt'(x, n, [])
val i = fn B0 => 0 | B1 => 1
val i_ = fn x => (case x mod 2 of 0 => B0 | 1 => B1 | _ => raise Match)
val drawIdCell = fn
  (x,y,fillrect,[a,b,c,d,e,f,g,h]) =>
    let
    val color = List.nth(idColors,(i a*8+i b*4+i c*2+i d*1))
    val shape = List.nth(idShapes,(i e*8+i f*4+i g*2+i h*1))
    in (fillrect(x,y,idCellWidth,idCellWidth,color)
       ;drawIdShape((x,y,fillrect,shape),0))
    end
| _ => raise Match 
val rec drawIdRow = fn
  (x,y,_,[]) => ()
 |(x,y,fillrect,xs) =>
    let val (s,e) = splitAt(xs,8)
    in (drawIdCell(x,y,fillrect,s);drawIdRow(x+idCellWidth,y,fillrect,e))
    end
val rec drawId = fn
  (_,_,fillrect,[]) => ()
 |(x,y,fillrect,xs) =>
    let val (s,e) = splitAt(xs,8*4)
    in (drawIdRow (x,y,fillrect,s); drawId(x,y+idCellWidth,fillrect,e))
    end


val drawBorder = fn(fillrect, x, y, w, h, size, color) =>
  (fillrect(x,        y,        w,      size, color)
  ;fillrect(x,        y+h-size, w,      size, color)
  ;fillrect(x,        y,        size,   h,    color)
  ;fillrect(x+w-size, y,        size,   h,    color)
  )

val rec relationToBox' = fn
  (K2.RComposition l, selectedPath, path) =>
    let val (x, h, boxes) = rCompositionToBoxes (selectedPath, path, 0, l, borderWidth, 0, [])
        val totalWidth = case l of [] => borderWidth*2 | _ => x
        val totalHeight = h+borderWidth*2
    in
      Box(
        totalWidth,
        totalHeight,
        fn fillrect => (
          case l of
            [] => ()
          | _  =>
            fillrect(
              borderWidth,
              borderWidth,
              totalWidth-borderWidth*2,
              totalHeight-borderWidth*2,
              compositionInnerColor
            );
          drawBorder(
            fillrect,
            0,
            0,
            totalWidth,
            totalHeight,
            borderWidth,
            compositionColor
          );
          map
          (fn (x, y, Box(w, h, _, _)) =>
            fillrect(
              x+w,
              y,
              borderWidth,
              totalHeight-borderWidth*2,
              compositionColor
            )
          )
          boxes;
          drawBorder(
            fillrect,
            0,
            0,
            totalWidth,
            totalHeight,
            edgeWidth,
            case path=selectedPath of true => selectColor | _ => edgeColor
          )
        ),
        boxes
      )
    end
| (K2.RProduct l, selectedPath, path) =>
    let val (y,w,boxes) = pkvsToBoxes(selectedPath, path, 0, l, borderWidth, 0, [])
        val totalWidth = w+borderWidth*2
        val totalHeight = case l of [] => borderWidth*2 | _ => y
    in
      Box(
        totalWidth,
        totalHeight,
        fn fillrect => (
          case l of
            [] => ()
          | _  =>
            fillrect(
              borderWidth,
              borderWidth,
              totalWidth-borderWidth*2,
              totalHeight-borderWidth*2,
              productInnerColor
            );
          drawBorder(
            fillrect,
            0,
            0,
            totalWidth,
            totalHeight,
            borderWidth,
            productColor
          );
          map
          (fn (i, (x, y, Box(w, h, _, _))) => (
            fillrect(
              x,
              y+h,
              totalWidth-borderWidth*2,
              borderWidth,
              productColor
            );
            case selectedPath = path @ [K2.RPEIndex i, K2.RPEProductLabel] of
              (* TODO fix this box being cut off *)
              true =>
                drawBorder(
                  fillrect,
                  x-edgeWidth,
                  y-edgeWidth,
                  idCellWidth*4+edgeWidth*2,
                  idCellWidth*4+edgeWidth*2,
                  edgeWidth,
                  selectColor
                )
            | _ => ()
          ))
          (List2.index (rev boxes));
          drawBorder(
            fillrect,
            0,
            0,
            totalWidth,
            totalHeight,
            edgeWidth,
            case path=selectedPath of true => selectColor | _ => edgeColor
          )
        ),
        boxes
      )
    end
| (K2.RRef r,        selectedPath, path) => refToBox(r, refColor,         path, selectedPath)
| (K2.RConstruct r,  selectedPath, path) => refToBox(r, constructColor,   path, selectedPath)
| (K2.RProjection r, selectedPath, path) => refToBox(r, projectionColor , path, selectedPath)
| (K2.RUnion l, selectedPath, path) =>
    let val (y,w,boxes) = rUnionToBoxes(selectedPath, path, 0, l,borderWidth,0,[])
        val totalWidth = w+borderWidth*2
        val totalHeight = case l of [] => borderWidth*2 | _ => y
    in
      Box(
        totalWidth,
        totalHeight,
        fn fillrect => (
          case l of
            [] => () 
          | _  =>
            fillrect(
              borderWidth,
              borderWidth,
              totalWidth-borderWidth*2,
              totalHeight-borderWidth*2,
              unionInnerColor
            );
          drawBorder(
            fillrect,
            0,
            0,
            totalWidth,
            totalHeight,
            borderWidth,
            unionColor
          );
          map 
          (fn (x,y,Box(w,h,_,_)) =>
            fillrect(
              x,
              y+h,
              totalWidth-borderWidth*2,
              borderWidth,
              unionColor
            )
          )
          boxes;
          drawBorder(
            fillrect,
            0,
            0,
            totalWidth,
            totalHeight,
            edgeWidth,
            case path=selectedPath of true => selectColor | _ => edgeColor
          )
        ),
        boxes
      )
    end
| _ => raise Match
and rCompositionToBoxes = fn
  (selectedPath, path, _, [], x, h, boxes)    => (x, h, boxes)
| (selectedPath, path, i, r::rs, x, h, boxes) =>
    let
      val box as Box(bw, bh, _, _) =
        relationToBox' (
          r,
          selectedPath,
          List.concat [path, [K2.RPEIndex i]]
        )
    in 
      rCompositionToBoxes(
        selectedPath,
        path,
        i+1,
        rs,
        x+bw+borderWidth,
        Int.max(h, bh),
        (x, borderWidth, box) :: boxes
      )
    end
and rUnionToBoxes = fn
  (selectedPath, path, _, [], y, w, boxes)    => (y, w, boxes)
| (selectedPath, path, i, r::rs, y, w, boxes) =>
    let
      val box as Box(bw, bh, _, _) =
        relationToBox' (
          r,
          selectedPath,
          List.concat [path, [K2.RPEIndex i]]
        )
    in 
      rUnionToBoxes(
        selectedPath,
        path,
        i+1,
        rs,
        y+bh+borderWidth,
        Int.max(w, bw),
        (borderWidth, y, box) :: boxes
      )
    end
and pkvsToBoxes = fn
  (selectedPath, path, _, [], y, w, boxes)              => (y, w, boxes)
| (selectedPath, path, i, (id, r) :: pkvs, y, w, boxes) =>
    let
      val box as Box(rw, rh, _, _) =
        relationToBox' (
          r,
          selectedPath,
          List.concat [path, [K2.RPEIndex i, K2.RPEProductRel]]
        )
      val ren = fn fillrect =>
        drawId(0, 0, fillrect, id)
      val bw = idCellWidth*4+rw
      val bh = Int.max(idCellWidth*4, rh)
    in
      pkvsToBoxes(
        selectedPath,
        path,
        i+1,
        pkvs,
        y+bh+borderWidth,
        Int.max(w, bw),
        (borderWidth, y, Box(bw, bh, ren, [(idCellWidth*4, 0, box)])) :: boxes
      )
    end
and relationToBox : K2.Relation * K2.RelationPathElem list -> Box = fn
  (r, path) => relationToBox' (r, path, [])
and refToBox = fn
  (id, color, path, selectedPath) =>
    let
      val totalWidth=borderWidth*2+idCellWidth*4
      val totalHeight= borderWidth*2+idCellWidth*4
    in
      Box(
        totalWidth,
        totalHeight,
        fn fillrect => (
          fillrect(
            0,
            0,
            totalWidth,
            totalHeight,
            color
          );
          drawId(borderWidth, borderWidth, fillrect, id);
          drawBorder(
            fillrect,
            0,
            0,
            totalWidth,
            totalHeight,
            edgeWidth,
            case path=selectedPath of true => selectColor | _ => edgeColor
          );
          case selectedPath = path @ [K2.RPEId] of
            true =>
              drawBorder(
                fillrect,
                borderWidth-edgeWidth,
                borderWidth-edgeWidth,
                idCellWidth*4+edgeWidth*2,
                idCellWidth*4+edgeWidth*2,
                edgeWidth,
                selectColor
            )
          | _ => ()
        ),
        []
      )
    end

val lol = BinIO.openIn "/dev/urandom"
val rec randomBitString = fn
  0 => []
 |n => i_(Word8.toInt(Option2.unSOME(BinIO.input1 lol)))::randomBitString(n-1)
val rec randomByteString : int -> Word8.word list = fn
  0 => []
 |n => valOf (BinIO.input1 lol)::randomByteString(n-1)
val randomByte : {} -> Word8.word = fn _ => valOf (BinIO.input1 lol)

val _ = case input_init() of 1 => () | _ => raise Fail "input_init failed"

val input_keyboard = fn() =>
  case input_keyboard_c() of
    ret => (
      (
        Int32.toInt(MLton.Pointer.getInt32(ret, 0)),
        (
          Int32.toInt(MLton.Pointer.getInt32(ret, 1)),
          Int32.toInt(MLton.Pointer.getInt32(ret, 2)),
          Int32.toInt(MLton.Pointer.getInt32(ret, 3))
        )
      )
    )

val input_mouse = fn() =>
  case input_mouse_c() of
    ret => (
      (
        Int32.toInt(MLton.Pointer.getInt32(ret, 0)),
        (
          Int32.toInt(MLton.Pointer.getInt32(ret, 1)),
          Int32.toInt(MLton.Pointer.getInt32(ret, 2)),
          Int32.toInt(MLton.Pointer.getInt32(ret, 3)),
          Int32.toInt(MLton.Pointer.getInt32(ret, 4))
        )
      )
    )

val rec mouse = fn(relation) => (
  case input_mouse() of
    (0, _) => raise Fail "input_mouse failed"
  | (2, _) => relation
  | (1, (xd, yd, click, altClick)) => (
      mouse(relation)
    )
  | _ => raise Match
)

datatype Action = 
  AReplaceWithUnion 
| AReplaceWithProduct
| AReplaceWithComposition
| AReplaceWithReference
| AReplaceWithProjection
| AReplaceWithConstruct
| AEncloseWithUnion 
| AEncloseWithProduct
| AEncloseWithComposition
| AEncloseWithReference
| AIn
| AOut
| AAddChildAfter
| AAddNeighborAbove
| AAddNeighborBelow
| AAddNeighborLeft
| AAddNeighborRight
| AMoveUp
| AMoveDown
| AMoveRight
| AMoveLeft
| AQuit
| ACopy
| APaste
| ANextMonitor

(* GUI State *)
structure S = struct
  val path = ref [K2.RPEIndex 1]
  val relation = ref (K2.RUnion [K2.RComposition [], K2.RComposition []])
  val updateScreen = ref true
  val quit = ref false
  val clip = ref (K2.RN.Rel (K2.RUnion []))
  val relationAliases = ref ()
  val labelAliases = ref ()
  val nextMonitor = ref false
end

signature TRIEMAP = sig
  type t
  type v
  val empty : t
  val insert : Bit list * v * t -> t
  val get : Bit list * t -> v option
end

functor TrieMap(T : sig type v end)  = struct
  type t = unit
  type v = T.v
  val empty = ()
  val insert = fn (_,_,_) => raise Match
  val get = fn (_,_) => raise Match
end : TRIEMAP

datatype Glyph = Glyph of bool list
structure GlyphTrieMap = TrieMap(struct type v = Glyph end)

datatype MovementDirection = MDUpDown | MDLeftRight | MDNone

val movementDirection = fn(path, relation) =>
  case K2.parentRelation(!S.path, !S.relation) of
    NONE => MDNone
  | SOME x =>
      case x of
        (K2.RUnion _) => MDUpDown
      | (K2.RProduct _) => MDUpDown
      | (K2.RComposition _) => MDLeftRight
      | (K2.RConstruct _) => MDNone
      | (K2.RProjection _) => MDNone
      | (K2.RRef _) => MDNone
      | _ => raise Match

val addNeighbor = fn (io, path, elem, root) =>
  let
    val (xs, f) =
      case rev path of
        (K2.RPEProductLabel :: K2.RPEIndex i :: xs) => (
          xs,
          fn K2.RN.Rel (K2.RProduct xs) =>
              (K2.RN.Rel o K2.RProduct o Option2.unSOME o List2.insertAt)
              (i+io, (randomBitString 128, elem), xs)
        )
      | (K2.RPEProductRel :: K2.RPEIndex i :: xs) => (
          xs,
          fn K2.RN.Rel (K2.RProduct xs) =>
              (K2.RN.Rel o K2.RProduct o Option2.unSOME o List2.insertAt)
              (i+io, (randomBitString 128, elem), xs)
        )
      | (K2.RPEIndex i :: xs) => (
          xs,
          fn
            K2.RN.Rel (K2.RUnion xs) =>
              (K2.RN.Rel o K2.RUnion o Option2.unSOME o List2.insertAt)
              (i+io, elem, xs)
          | K2.RN.Rel (K2.RComposition xs) =>
              (K2.RN.Rel o K2.RComposition o Option2.unSOME o List2.insertAt)
              (i+io, elem, xs)
          | x => x
        )
      | _ => raise Match
  in
    K2.modifyRelationAt (rev xs, f, root)
  end


val moveForward :
  (K2.RelationPathElem list * K2.Relation)
-> K2.RelationPathElem list = fn (path, root) =>
  case K2.parentRelation (path, root) of
    NONE => path
  | SOME x =>
      let
        val l = case x of
          K2.RUnion xs => List.length xs
        | K2.RProduct xs => List.length xs
        | K2.RComposition xs => List.length xs
        | _ => raise Match
      in
        case rev path of
          (K2.RPEProductLabel :: K2.RPEIndex x :: xs) =>
            if x+1 < l
            then rev (K2.RPEProductLabel :: K2.RPEIndex (x+1) :: xs)
            else path
        | (K2.RPEProductRel :: K2.RPEIndex x :: xs) =>
            if x+1 < l
            then rev (K2.RPEProductRel :: K2.RPEIndex (x+1) :: xs)
            else path
        | (K2.RPEIndex x :: xs) =>
            if x+1 < l
            then rev (K2.RPEIndex (x+1) :: xs)
            else path
        | _ => raise Match
      end

val moveBackward : K2.RelationPathElem list -> K2.RelationPathElem list = fn path =>
  case rev path of
    K2.RPEIndex 0 :: xs =>
      path
  | K2.RPEIndex x :: xs =>
      rev (K2.RPEIndex (x-1) :: xs)
  | K2.RPEProductLabel :: K2.RPEIndex 0 :: xs =>
      path
  | K2.RPEProductLabel :: K2.RPEIndex x :: xs =>
      rev (K2.RPEProductLabel :: K2.RPEIndex (x-1) :: xs)
  | K2.RPEProductRel :: K2.RPEIndex 0 :: xs =>
      path
  | K2.RPEProductRel :: K2.RPEIndex x :: xs =>
      rev (K2.RPEProductRel :: K2.RPEIndex (x-1) :: xs)

val actions = [
  (AReplaceWithUnion, fn _ => (
    S.relation := K2.replaceRelationAt(!S.path, K2.RN.Rel (K2.RUnion []), !S.relation);
    S.updateScreen := true
  )),
  (AReplaceWithProduct, fn _ => (
    S.relation := K2.replaceRelationAt(!S.path, K2.RN.Rel (K2.RProduct []), !S.relation);
    S.updateScreen := true
  )),
  (AReplaceWithComposition, fn _ => (
    S.relation := K2.replaceRelationAt(!S.path, K2.RN.Rel (K2.RComposition []), !S.relation);
    S.updateScreen := true
  )),
  (AReplaceWithReference, fn _ => (
    S.relation := K2.replaceRelationAt(
      !S.path,
      (K2.RN.Rel o K2.RRef o randomBitString) 128,
      !S.relation);
    S.updateScreen := true
  )),
  (AReplaceWithProjection, fn _ => (
    S.relation := K2.replaceRelationAt(
      !S.path,
      (K2.RN.Rel o K2.RProjection o randomBitString) 128,
      !S.relation);
    S.updateScreen := true
  )),
  (AReplaceWithConstruct, fn _ => (
    S.relation := K2.replaceRelationAt(
      !S.path,
      (K2.RN.Rel o K2.RConstruct o randomBitString) 128,
      !S.relation);
    S.updateScreen := true
  )),
  (AEncloseWithUnion, fn _ => (
    S.relation := K2.modifyRelationAt(
      !S.path,
      fn
        K2.RN.Rel r => (
          S.path := List.concat [!S.path, [K2.RPEIndex 0]];
          S.updateScreen := true;
          (K2.RN.Rel o K2.RUnion) [r]
        )
      | r => r,
      !S.relation)
  )),
  (AEncloseWithProduct, fn _ =>
    S.relation := K2.modifyRelationAt(
      !S.path,
      fn
        K2.RN.Rel r => (
          S.path := (!S.path @ [K2.RPEIndex 0, K2.RPEProductRel]);
          S.updateScreen := true;
          (K2.RN.Rel o K2.RProduct) [(randomBitString 128, r)]
        )
      | r => r,
      !S.relation)
  ),
  (AEncloseWithComposition, fn _ => (
    S.relation := K2.modifyRelationAt(
      !S.path,
      fn
        K2.RN.Rel r => (
          S.path := List.concat [!S.path, [K2.RPEIndex 0]];
          S.updateScreen := true;
          (K2.RN.Rel o K2.RComposition) [r]
        )
      | r => r,
      !S.relation)
  )),
  (AEncloseWithReference, fn _ => (
    S.relation := K2.replaceRelationAt(
      !S.path,
      (K2.RN.Rel o K2.RRef o randomBitString) 128,
      !S.relation);
    S.updateScreen := true
  )),
  (AIn, fn _ => (
    let
      val f = fn
        [] => ()
      | _ => S.path := List.concat [!S.path, [K2.RPEIndex 0]]
      val g = fn _ => S.path := ((!S.path) @ [K2.RPEId])
    in
      case K2.relationAt(!S.path, !S.relation) of
        K2.RN.LabelId _ => ()
      | K2.RN.RelId _ => ()
      | K2.RN.Rel (K2.RUnion xs) => f xs
      | K2.RN.Rel (K2.RComposition xs) => f xs
      | K2.RN.Rel (K2.RProduct []) => ()
      | K2.RN.Rel (K2.RProduct xs) =>
          S.path := (!S.path @ [K2.RPEIndex 0, K2.RPEProductRel])
      | K2.RN.Rel (K2.RRef _) => g()
      | K2.RN.Rel (K2.RConstruct _) => g()
      | K2.RN.Rel (K2.RProjection _) => g() 
      | _ => raise Match
    end;
    S.updateScreen := true
  )),
  (AOut, fn _ => (
    case
      case rev (!S.path) of
        [] => []
      | (K2.RPEProductLabel :: _ :: xs) => xs
      | (K2.RPEProductRel :: _ :: xs) => xs
      | (_ :: xs) => xs
    of
      xs => S.path := rev xs;
    S.updateScreen := true
  )),
  (AAddNeighborAbove, fn _ => (
    case movementDirection(!S.path, !S.relation) of
      MDUpDown => (
        S.relation := addNeighbor(0, !S.path, K2.RUnion [], !S.relation);
        case rev (!S.path) of
          (K2.RPEIndex x::xs) =>
            S.path := rev (K2.RPEIndex (x+1) :: xs)
        | (K2.RPEProductLabel  :: K2.RPEIndex x :: xs) =>
            S.path := rev (K2.RPEProductLabel :: K2.RPEIndex (x+1) :: xs)
        | (K2.RPEProductRel  :: K2.RPEIndex x :: xs) =>
            S.path := rev (K2.RPEProductRel :: K2.RPEIndex (x+1) :: xs)
        | _ => raise Match;
        S.updateScreen := true
      )
    | _ => ()
  )),
  (AAddNeighborLeft, fn _ => (
    case movementDirection(!S.path, !S.relation) of
      MDLeftRight => (
        S.relation := addNeighbor(0, !S.path, K2.RUnion [], !S.relation);
        case rev (!S.path) of
          (K2.RPEIndex x::xs) =>
            S.path := rev (K2.RPEIndex (x+1) :: xs)
        | (K2.RPEProductLabel  :: K2.RPEIndex x :: xs) =>
            S.path := rev (K2.RPEProductLabel :: K2.RPEIndex (x+1) :: xs)
        | (K2.RPEProductRel  :: K2.RPEIndex x :: xs) =>
            S.path := rev (K2.RPEProductRel :: K2.RPEIndex (x+1) :: xs)
        | _ => raise Match;
        S.updateScreen := true
      )
    | _ => ()
  )),
  (AAddNeighborBelow, fn _ => (
    case movementDirection(!S.path, !S.relation) of
      MDUpDown => (
        S.relation := addNeighbor(1, !S.path, K2.RUnion [], !S.relation);
        S.updateScreen := true
      )
    | _ => ()
  )),
  (AAddNeighborRight, fn _ => (
    case movementDirection(!S.path, !S.relation) of
      MDLeftRight => (
        S.relation := addNeighbor(1, !S.path, K2.RUnion [], !S.relation);
        S.updateScreen := true
      )
    | _ => ()
  )),
  (AMoveLeft, fn _ => (
    case movementDirection(!S.path, !S.relation) of
      MDLeftRight => (
        S.path := moveBackward (!S.path);
        S.updateScreen := true
      )
    | _ =>
        case rev (!S.path) of
          (K2.RPEProductRel :: xs) => (
            S.path := rev (K2.RPEProductLabel :: xs);
            S.updateScreen := true
          )
        | _ => ()
  )),
  (AMoveRight, fn _ => (
    case movementDirection(!S.path, !S.relation) of
      MDLeftRight => (
        S.path := moveForward (!S.path, !S.relation);
        S.updateScreen := true
      )
    | _ =>
        case rev (!S.path) of
          (K2.RPEProductLabel :: xs) => (
            S.path := rev (K2.RPEProductRel :: xs);
            S.updateScreen := true
          )
        | _ => ()
  )),
  (AMoveUp, fn _ => (
    case movementDirection(!S.path, !S.relation) of
      MDUpDown => (
        S.path := moveBackward (!S.path);
        S.updateScreen := true
      )
    | _ => ()
  )),
  (AMoveDown, fn _ => (
    case movementDirection(!S.path, !S.relation) of
      MDUpDown => (
        S.path := moveForward (!S.path, !S.relation);
        S.updateScreen := true
      )
    | _ => ()
  )),
  (AQuit, fn _ => (
    S.quit := true
  )),
  (ACopy, fn _ => (
    S.clip := K2.relationAt(!S.path, !S.relation)
  )),
  (APaste, fn _ => (
    S.relation := K2.replaceRelationAt(!S.path, !S.clip, !S.relation);
    S.updateScreen := true
  )),
  (ANextMonitor, fn _ => (
    S.nextMonitor := true
  ))
]

datatype KeyTriggerType = KeyTriggerOnPress | KeyTriggerWhileHeld

(* Key codes defined in /usr/include/linux/input-event-codes.h *)
val actionKeys = [
  (2, [2,29], KeyTriggerOnPress, AEncloseWithUnion),
  (3, [3,29], KeyTriggerOnPress, AEncloseWithProduct),
  (5, [5,29], KeyTriggerOnPress, AEncloseWithComposition),
  (7, [7,29], KeyTriggerOnPress, AEncloseWithReference),
  (2, [2], KeyTriggerOnPress, AReplaceWithUnion),
  (3, [3], KeyTriggerOnPress, AReplaceWithProduct),
  (4, [4], KeyTriggerOnPress, AReplaceWithConstruct),
  (5, [5], KeyTriggerOnPress, AReplaceWithComposition),
  (6, [6], KeyTriggerOnPress, AReplaceWithProjection),
  (7, [7], KeyTriggerOnPress, AReplaceWithReference),
  (17, [17], KeyTriggerOnPress, AOut),
  (19, [19], KeyTriggerOnPress, AIn),
  (18, [18,29], KeyTriggerOnPress, AAddNeighborAbove),
  (32, [29,32], KeyTriggerOnPress, AAddNeighborBelow),
  (31, [29,31], KeyTriggerOnPress, AAddNeighborLeft),
  (33, [29,33], KeyTriggerOnPress, AAddNeighborRight),
  (18, [18], KeyTriggerOnPress, AMoveUp),
  (32, [32], KeyTriggerOnPress, AMoveDown),
  (33, [33], KeyTriggerOnPress, AMoveRight),
  (31, [31], KeyTriggerOnPress, AMoveLeft),
  (43, [29,43], KeyTriggerOnPress, AQuit),
  (46, [29,46], KeyTriggerOnPress, ACopy),
  (47, [29,47], KeyTriggerOnPress, APaste),
  (28, [28,29,56], KeyTriggerOnPress, ANextMonitor)
]

val keyStates = Array.array (1000, false) (* Arbitrary limit. Higher keycodes will be ignored *)

val heldKeys = fn() =>
  Array.foldri
  (fn
    (keyCode, true, xs) => keyCode::xs
  | (_, false, xs) => xs
  )
  []
  keyStates

val rec keyboard = fn() =>
  case input_keyboard() of
    (0, _) => raise Fail "input_keyboard failed"
  | (2, (type_, code, value)) => ()
  | (1, (type_, code, 0)) => (
      Array.update(keyStates, code, false);
      ()
    )
  | (1, (type_, code, 1)) => (
      Array.update(keyStates, code, true);
      keyboard'(code, KeyTriggerOnPress)
    )
  | (1, (type_, code, 2)) => (
      Array.update(keyStates, code, true);
      keyboard'(code, KeyTriggerWhileHeld)
    )
  | _ => raise Match
  and keyboard' = fn(code, triggeredType) =>
    let val heldKeys = heldKeys() in
      case List.find
           (fn(keyCode, holdKeys, triggerType, _) =>
             (keyCode, triggerType) = (code, triggeredType) andalso
             holdKeys = heldKeys
           )
           actionKeys
      of
        NONE => keyboard()
      | SOME(_, _, _, a) =>
          keyboard(
            case List.find (fn (action, _) => action=a) actions
            of
              NONE => ()
             |SOME(_, f) => f ()
          )
    end

val rec setup_monitor : unit -> bool = fn _ => (
  mouse();
  keyboard();
  if !S.quit
  then true
  else
    case fb_init() of
      ~1 => setup_monitor ()
    | 0 => (fb_clear (w32i backgroundColor); fb_copy(); false)
      (* XXX Clear the framebuffer immediately because DRM-KMS stuff just reuses some existing memory
       *     with some arbitrary contents, often displaying a window you were previously viewing,
       *     like a terminal or web browser.
       *     This workaround isn't even a full fix since the uninitialized buffer may still be displayed
       *     for a short period of time before this code takes effect.
       *     Also this sucks and just causes even more unnecessary delays and flashing/tearing of various contents.
       *     Is this expected behavior of DRM-KMS or a bug in the kernel or some driver?
       *     Maybe the API has some option I missed to give us an initialized buffer.
       **)
)
val rec showAllIdColors = fn
  16 => ()
 |i => (
    fb_fillrect(
      w32i(10+(i mod 4)*idCellWidth),
      w32i(500+(i div 4)*idCellWidth),
      w32i(idCellWidth),
      w32i(idCellWidth),
      w32i(List.nth(idColors,i))
    );
    showAllIdColors(i+1)
  )

val rec main = fn () => (
  mouse();
  keyboard();
  case !S.quit of
    true => ()
  | false =>
      case !S.nextMonitor of
        false => (
          case !S.updateScreen of
            false => ()
          | true => (
              fb_clear (w32i backgroundColor);
              renderBox(0, 0, relationToBox(!S.relation, !S.path));
              fb_wait_vblank();
              fb_copy();
              S.updateScreen := false
          );
          main()
        )
      | true =>
          case setup_monitor() of
            true => ()
          | false => (S.nextMonitor := false; S.updateScreen := true; main())
)

val glyphColor = 0
val glyphWidth = 15
val glyphHeight = 15

(* TODO: perhaps this should be 15x15 `Box` structures to be consistent *)
val glyphToBox = fn(g, scale, path, selectedPath) =>
  Box(
    glyphWidth,
    glyphHeight,
    fn fillrect => (
      List.foldl
      (fn ((xi, yi), x) =>
        let
          val (xi', yi') = case xi+1=glyphWidth of
                             true => (0, yi+scale)
                           | false => (xi+scale, yi)
        in
          fillrect(xi, yi, scale, scale, glyphColor);
          (xi', yi')
        end)
      (0, 0)
      g;
      ()
    ),
    []
  )

val _ =
  case setup_monitor () of
    false => main ()
  | true => ()


val checkPreviousEntries = fn (f,l) =>
  map
    (
      fn (k,v) =>
        case
          Array.foldl
            (
              fn
                (_,[]) =>
                  raise Fail "checkPreviousEntries value too long"
              | (a,x::xs) =>
                if a = x
                then
                  xs
                else
                  raise Fail "checkPreviousEntries value not equal"
            )
            v
            (
              case OnDiskKVStore.get {file=f, key=k} of
                NONE => raise Fail "checkPreviousEntries missing entry"
              | SOME x => x
            )
          of
            [] => {}
          | _ => raise Fail "checkPreviousEntries value too short"
    )
    l

val rec testOnDiskKVStore = fn
  (f,l,0) => {}
| (f,l,i) =>
  let
    val k = randomBitString (Int.max (Word8.toInt (randomByte {}) mod 512,128))
    val v = randomByteString (Word8.toInt (randomByte {} mod 0w100))
  in
    (checkPreviousEntries (f,l)
    ;OnDiskKVStore.put {file=f, key=k ,value=v}
    ;testOnDiskKVStore (f,(k,v)::l, i-1)
    )
  end

(*val _ = testOnDiskKVStore ([], 10000)*)

