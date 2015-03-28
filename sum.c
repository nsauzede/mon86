// check rom extension sum (and fix, if asked)
// (C) Nicolas Sauzede 2009 (nsauzede@laposte.net)

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define dprintf(...)

int main( int argc, char *argv[])
{
	char *name = NULL;
	FILE *in;
	int mod = 0;

	if (argc > 1)
	{
		name = argv[1];
		if (argc > 2)
		{
			if (!strcmp(argv[2],"mod"))
				mod = 1;
		}
	}
	in = fopen( name, "rb");
	if (!in)
	{
		printf( "couldn't open '%s' !\n", name);
	}
	else
	{
		long sizef, i;
		unsigned char *buf;
		
		fseek( in, 0, SEEK_END);
		sizef = ftell( in);
		if (sizef < 3)
		{
			printf( "file is too small !\n");
			exit( 42);
		}
		dprintf( "file size is %ld\n", sizef);
		rewind( in);
		buf = malloc( sizef);
		if (buf)
		{
			int sum = 0;
			char sizes;
			long sizeb;
			int should_mod = 0;
			unsigned char pad = 0;

			fread( buf, sizef, 1, in);
			if ((buf[0] != 0x55) || (buf[1] != 0xAA))
			{
				printf( "file is not a ROM extension !\n");
				exit( 42);
			}
			sizes = buf[2];
			dprintf( "stored size=%d\n", sizes);
			if (!sizes)
				sizes = sizef / 512 + 1;
			dprintf( "computed size=%d\n", sizes);
			sizeb = sizes * 512;
			if (sizef < sizeb)
			{
				void *old = buf;
				buf = malloc( sizeb);
				memset( buf, 0, sizeb);
				memcpy( buf, old, sizef);
				free( old);
				sizef = sizeb;
			}
			for (i = 0; i < sizeb; i++)
			{
				unsigned char c;

				c = buf[i];
				sum += c;
			}
			dprintf( "computed sum=%04x : ", sum);
			if (sum % 256)
			{
				pad = -sum;
				printf( "sum mismatch should be %04X\n", (unsigned short)pad);
				should_mod = 1;
			}
			else
			{
				dprintf( "sum match\n");
			}
			if (should_mod)
			{
				if (mod)
				{
					fclose( in);
					in = fopen( name, "rb+");
					buf[2] = sizes;
					buf[sizeb-2] = 0;
					buf[sizeb-1] = pad;
					fwrite(buf,sizef,1,in);
					dprintf( "moded\n");
				}
				else
					printf( "invalid ROM, please use mod argument to fix\n");
			}
			else
				printf( "file is a valid ROM extension\n");
			free( buf);
		}
		fclose( in);
	}

	return 0;
}

