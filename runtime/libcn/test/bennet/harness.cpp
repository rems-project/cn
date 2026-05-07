#include "harness.hpp"

#include <gtest/gtest.h>

#include <bennet/prelude.h>

void LibBennet::SetUp() {
  bennet_init();
  bennet_srand(0);
  bennet_set_size(20);
  bennet_set_max_size(25);
  bennet_set_depth(0);
}

void LibBennet::TearDown() {
  bennet_destroy();
}
