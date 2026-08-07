/**
 * @file test_domain_product.hpp
 * @brief The gtest binary's hand-written bennet_domain(ty) product struct.
 *
 * libbennet.a contains assign.c/failure.c but not the OCaml-generated
 * product type they operate on; ownership_domain.cpp supplies the function
 * definitions and this header the struct shape, shared so tests can inspect
 * the elements. Two all-ownership components mirror domain.ml's generated
 * layout closely enough that ASan redzones catch D6-class over-reads: a
 * bare ownership struct passed where the product is expected under-allocates
 * element_1.
 */
#ifndef TEST_DOMAIN_PRODUCT_HPP
#define TEST_DOMAIN_PRODUCT_HPP

#include <bennet/internals/domain.h>
#include <bennet/internals/domains/ownership.h>

extern "C" {

#define TEST_BENNET_DOMAIN_STRUCT(ty)                                                    \
  bennet_domain(ty) {                                                                    \
    bennet_domain_ownership(ty) element_0;                                               \
    bennet_domain_ownership(ty) element_1;                                               \
  };

TEST_BENNET_DOMAIN_STRUCT(int8_t)
TEST_BENNET_DOMAIN_STRUCT(uint8_t)
TEST_BENNET_DOMAIN_STRUCT(int16_t)
TEST_BENNET_DOMAIN_STRUCT(uint16_t)
TEST_BENNET_DOMAIN_STRUCT(int32_t)
TEST_BENNET_DOMAIN_STRUCT(uint32_t)
TEST_BENNET_DOMAIN_STRUCT(int64_t)
TEST_BENNET_DOMAIN_STRUCT(uint64_t)
TEST_BENNET_DOMAIN_STRUCT(uintptr_t)

#undef TEST_BENNET_DOMAIN_STRUCT
}

#endif  // TEST_DOMAIN_PRODUCT_HPP
