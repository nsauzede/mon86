// transform the given bin file to a rom extension
// (C) Nicolas Sauzede 2009 (nsauzede@laposte.net)

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

//#define DEBUG

// input file must begin with 0x55,0xAA,0
// and should not contain any checksum
// total size will be padded to multiple of 512

#ifdef DEBUG
#define dprintf(...) do{printf(__VA_ARGS__);fflush(stdout);}while(0)
#else
#define dprintf(...)
#endif

int main( int argc, char *argv[])
{
	char *name = NULL, *name2 = NULL;
	FILE *in;

	if (argc > 1)
	{
		name = argv[1];
		if (argc > 2)
		{
			name2 = argv[2];
		}
	}
	if (!name2 || !name)
	{
		printf( "usage: mkrom in out\n");
		exit( 1);
	}
	dprintf( "input is [%s] output is [%s]\n", name, name2);
	in = fopen( name, "rb+");
	if (in)
	{
		long size, i;
		unsigned char *buf;
		unsigned char sizeread, sizecomp;
		
		fseek( in, 0, SEEK_END);
		size = ftell( in);
		dprintf( "file size=%ld\n", size);
		if (size < 3)
		{
			printf( "file is too small to read !\n");
			exit( 42);
		}
		rewind( in);
		buf = malloc( size);
		if (buf)
		{
			int sum = 0x00;

			fread( buf, size, 1, in);
			if ((buf[0] != 0x55) || (buf[1] != 0xAA))
			{
				printf( "file is not a ROM extension !\n");
				exit( 42);
			}
			sizeread = buf[2];
			sizeread = sizeread;
			dprintf( "stored size=%d\n", sizeread);
//			sizecomp = (size + 511) / 512;
			sizecomp = (size ) / 512 + 1;
//			if (!sizecomp)
//				sizecomp++;
			dprintf( "computed size=%d\n", sizecomp);
			buf[2] = sizecomp;
			long ssize = size;
//			if (size == 512)
//				ssize--;
			for (i = 0; i < ssize; i++)
			{
				unsigned char c;
				
				c = buf[i];
				sum += c;
			}
			dprintf( "computed sum=%04x : ", sum);
			unsigned char pad = 0;
			if (sum % 256)
			{
				pad = -(sum % 256);
				dprintf( "sum mismatch should be %04X\n", (unsigned short)pad);
			}
			else
			{
				dprintf( "sum match\n");
			}

			FILE *out = fopen( name2, "wb");
			if (out)
			{
				long rsize = size;
				fwrite( buf, rsize, 1, out);
				int padsize = sizecomp * 512 - size - 1;
				dprintf( "padsize=%d bytes\n", padsize);
				if (padsize > 0)
				{
					char *buf2 = malloc( padsize);

					dprintf( "padding %d bytes\n", padsize);
					memset( buf2, 0x00, padsize);
					fwrite( buf2, padsize, 1, out);
					free( buf2);
				}
				fwrite( &pad, sizeof( pad), 1, out);
				fclose( out);
			}

			free( buf);
		}
		fclose( in);
	}

	return 0;
}

