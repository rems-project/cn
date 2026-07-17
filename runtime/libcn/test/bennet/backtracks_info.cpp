#include <gtest/gtest.h>

#include <bennet/info/backtracks.h>

TEST(BacktracksInfo, TotalAccumulatesAcrossRunsAndResets) {
  // Safe to query before the module is initialized
  EXPECT_EQ(bennet_info_backtracks_total(), 0);

  bennet_info_backtracks_init();
  bennet_info_backtracks_set_function_under_test("f");

  bennet_info_backtracks_begin_run();
  bennet_info_backtracks_log("gen", "file.c", 1);
  bennet_info_backtracks_log("gen", "file.c", 2);
  // Unrecorded attempts still count towards the running total
  bennet_info_backtracks_end_run(false);
  EXPECT_EQ(bennet_info_backtracks_last_total(), 2);
  EXPECT_EQ(bennet_info_backtracks_total(), 2);

  bennet_info_backtracks_begin_run();
  bennet_info_backtracks_log("gen", "file.c", 3);
  bennet_info_backtracks_end_run(true);
  EXPECT_EQ(bennet_info_backtracks_last_total(), 1);
  EXPECT_EQ(bennet_info_backtracks_total(), 3);

  bennet_info_backtracks_reset_total();
  EXPECT_EQ(bennet_info_backtracks_total(), 0);
}
