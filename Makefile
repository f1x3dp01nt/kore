2kore:	kore.sml drm.o input.o file.o
	mlton -default-ann 'allowFFI true' -link-opt -ldrm -link-opt -lkms kore.sml input.o drm.o file.o
	mv kore 2kore
input.o:	input.c
	gcc -O2 input.c -c -o input.o
file.o:	file.c
	gcc -O2 file.c -c -o file.o
drm.o:	drm.c
	gcc -O2 `pkg-config --cflags --libs libdrm libkms` drm.c -c -o drm.o
clean:
	rm -f drm.o 1kore 2kore
1kore:
	echo -e "#!/bin/bash\necho 'Segmentation fault'" > 1kore
	chmod 755 1kore
