#include <limits.h>
#include <stdio.h>
#define BYTE_TO_BITS 8

/*
	standard signed integer types:
		short int,
		int,
		long int,
		long long int
		signed char // default signdness varies
		
	standard signed integer types:
		unsigned short int,
		unsigned int,
		unsigned long int,
		unsigned long long int,
		unsigned char

	certain c compilers are allowed to have implementation-defined
	int types (AKA extended integer types), such as 128 bit ones.
*/
int main(void) {
	printf("DATATYPE SIZES & LIMITS:\n");
	
	printf("CHAR:\n");
	printf(" Bits: %d\n", CHAR_BIT);
	
	printf(" Signed:\n");
	printf("  Min Size: %d\n", SCHAR_MIN);
	printf("  Max Size: %d\n", SCHAR_MAX);

	printf(" Unsigned:\n");
	printf("  Min Size: 0\n");
	printf("  Max Size: %u\n", UCHAR_MAX);

	printf("\nSHORT:\n");
	printf(" Bits: %lu\n", sizeof(short) * BYTE_TO_BITS);

	printf(" Signed:\n");
	printf("  Min Size: %hd\n", SHRT_MIN);
	printf("  Max Size: %hd\n", SHRT_MAX);

	printf(" Unsigned:\n");
	printf("  Min Size: 0\n");
	printf("  Max Size: %hu\n", USHRT_MAX);
	
	printf("\nINT:\n");
	printf(" Bits: %lu\n", sizeof(int) * BYTE_TO_BITS);

	printf(" Signed:\n");
	printf("  Min Size: %d\n", INT_MIN);
	printf("  Max Size: %d\n", INT_MAX);

	printf(" Unsigned:\n");
	printf("  Min Size: 0\n");
	printf("  Max Size: %u\n", UINT_MAX);

	printf("\nLONG:\n");
	printf(" Bits: %lu\n", sizeof(long) * BYTE_TO_BITS);
	
	printf(" Signed:\n");
	printf("  Min: %ld\n", LONG_MIN);
	printf("  Max: %ld\n", LONG_MAX);

	printf(" Unsigned:\n");
	printf("  Min: 0\n");
	printf("  Max: %lu\n", ULONG_MAX);

	printf("\nLONG LONG:\n");
	printf(" Bits: %lu\n", sizeof(long long) * BYTE_TO_BITS);
	
	printf(" Signed:\n");
	printf("  Min: %lld\n", LLONG_MIN);
	printf("  Max: %lld\n", LLONG_MAX);

	printf(" Unsigned:\n");
	printf("  Min: 0\n");
	printf("  Max: %llu\n", ULLONG_MAX);
	return 0;
}
