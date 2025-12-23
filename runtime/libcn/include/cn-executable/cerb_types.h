// System dependent definitions of the basic Cerberus types.
#ifndef CERB_TYPES
#define CERB_TYPES

typedef unsigned char __cerbty_uint8_t;
typedef unsigned short __cerbty_uint16_t;
typedef unsigned int __cerbty_uint32_t;
typedef unsigned long long __cerbty_uint64_t;

typedef signed char __cerbty_int8_t;
typedef signed short __cerbty_int16_t;
typedef signed int __cerbty_int32_t;
typedef signed long long __cerbty_int64_t;

typedef long __cerbty_intmax_t;
typedef unsigned long __cerbty_uintmax_t;

typedef unsigned long __cerbty_uintptr_t;
typedef signed long __cerbty_intptr_t;
typedef signed long __cerbty_ptrdiff_t;
typedef unsigned long __cerbty_size_t;
typedef signed int __cerbty_wchar_t;

#define __cerbvar_MAX_ALIGNMENT 16
#define __cerbvar_NULL          0

#endif
