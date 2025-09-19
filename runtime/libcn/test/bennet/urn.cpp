#include "harness.hpp"
#include <gtest/gtest.h>
#include <unordered_set>

#include <algorithm>
#include <vector>

#include <bennet/internals/rand.h>
#include <bennet/internals/urn.h>

class UrnTest : public LibBennet {
 protected:
  void TearDown() override {
    // Clean up any urns created during tests
    if (test_urn) {
      urn_free(test_urn);
      test_urn = nullptr;
    }
  }

  struct bennet_int_urn* test_urn = nullptr;
};

// Test urn creation from array
TEST_F(UrnTest, FromArrayBasic) {
  uint64_t elems[] = {1, 10, 2, 20, 3, 30};
  test_urn = urn_from_array(elems, 3);

  ASSERT_NE(test_urn, nullptr);
  EXPECT_EQ(test_urn->size, 3);
  EXPECT_NE(test_urn->tree, nullptr);
  // Note: Can't access tree internals directly, so test via behavior
}

// Test urn creation with zero weights (should be skipped)
TEST_F(UrnTest, FromArrayWithZeroWeights) {
  uint64_t elems[] = {1, 10, 0, 20, 2, 30, 0, 40};
  test_urn = urn_from_array(elems, 4);

  ASSERT_NE(test_urn, nullptr);
  EXPECT_EQ(test_urn->size, 2);  // Only elements with non-zero weight
}

// Test urn creation from empty array
TEST_F(UrnTest, FromEmptyArray) {
  uint64_t elems[] = {};
  test_urn = urn_from_array(elems, 0);

  ASSERT_NE(test_urn, nullptr);
  EXPECT_EQ(test_urn->size, 0);
  EXPECT_EQ(test_urn->tree, nullptr);
}

// Test single element insertion
TEST_F(UrnTest, SingleInsertion) {
  uint64_t elems[] = {};
  test_urn = urn_from_array(elems, 0);

  urn_insert(test_urn, 5, 42);

  EXPECT_EQ(test_urn->size, 1);
  EXPECT_NE(test_urn->tree, nullptr);
  // Test behavior: single element should be removable
  bennet_srand(0);
  uint64_t removed = urn_remove(test_urn);
  EXPECT_EQ(removed, 42);
  EXPECT_EQ(test_urn->size, 0);
  EXPECT_EQ(test_urn->tree, nullptr);
}

// Test multiple insertions maintain balance
TEST_F(UrnTest, MultipleInsertions) {
  uint64_t elems[] = {};
  test_urn = urn_from_array(elems, 0);

  urn_insert(test_urn, 1, 10);
  urn_insert(test_urn, 2, 20);
  urn_insert(test_urn, 3, 30);
  urn_insert(test_urn, 4, 40);

  EXPECT_EQ(test_urn->size, 4);
  EXPECT_NE(test_urn->tree, nullptr);

  // Test behavior: all elements should be removable
  std::unordered_set<uint64_t> removed_values;
  while (test_urn->size > 0) {
    uint64_t removed = urn_remove(test_urn);
    removed_values.insert(removed);
  }
  EXPECT_EQ(removed_values.size(), 4);
  EXPECT_TRUE(removed_values.count(10));
  EXPECT_TRUE(removed_values.count(20));
  EXPECT_TRUE(removed_values.count(30));
  EXPECT_TRUE(removed_values.count(40));
}

// Test removal from single element urn
TEST_F(UrnTest, RemovalSingleElement) {
  uint64_t elems[] = {5, 42};
  test_urn = urn_from_array(elems, 1);

  // Set seed for deterministic behavior
  bennet_srand(0);
  uint64_t removed = urn_remove(test_urn);

  EXPECT_EQ(removed, 42);
  EXPECT_EQ(test_urn->size, 0);
  EXPECT_EQ(test_urn->tree, nullptr);
}

// Test removal from multi-element urn
TEST_F(UrnTest, RemovalMultipleElements) {
  uint64_t elems[] = {1, 10, 2, 20, 3, 30};
  test_urn = urn_from_array(elems, 3);

  uint8_t initial_size = test_urn->size;

  // Set seed for deterministic behavior
  bennet_srand(42);
  uint64_t removed = urn_remove(test_urn);

  EXPECT_EQ(test_urn->size, initial_size - 1);
  EXPECT_TRUE(removed == 10 || removed == 20 || removed == 30);
}

// Test that all elements can be removed
TEST_F(UrnTest, RemoveAllElements) {
  uint64_t elems[] = {1, 10, 1, 20, 1, 30};
  test_urn = urn_from_array(elems, 3);

  std::unordered_set<uint64_t> removed_values;

  // Remove all elements
  while (test_urn->size > 0) {
    uint64_t removed = urn_remove(test_urn);
    removed_values.insert(removed);
  }

  // Should have removed all original values
  EXPECT_EQ(removed_values.size(), 3);
  EXPECT_TRUE(removed_values.count(10));
  EXPECT_TRUE(removed_values.count(20));
  EXPECT_TRUE(removed_values.count(30));

  EXPECT_EQ(test_urn->size, 0);
  // Note: tree may not be null due to replacement algorithm
}

// Test weight distribution in sampling
TEST_F(UrnTest, WeightedSampling) {
  // Create an urn with heavily weighted element
  uint64_t elems[] = {1, 100, 99, 200};  // 200 should be sampled much more often
  test_urn = urn_from_array(elems, 2);

  const int num_samples = 1000;
  int count_100 = 0, count_200 = 0;

  // Take many samples to test weight distribution
  for (int i = 0; i < num_samples; i++) {
    bennet_srand(i);
    uint64_t sampled = urn_remove(test_urn);
    if (sampled == 100)
      count_100++;
    else if (sampled == 200)
      count_200++;

    // Re-insert the element to maintain urn state
    if (sampled == 100) {
      urn_insert(test_urn, 1, 100);
    } else if (sampled == 200) {
      urn_insert(test_urn, 99, 200);
    }
  }

  // 200 should be sampled roughly 99 times more often than 100
  // Allow for some variance in random sampling
  EXPECT_GT(count_200, count_100 * 50);                 // At least 50x more frequent
  EXPECT_GT(count_200 + count_100, num_samples * 0.9);  // Most samples should be valid
}

// Test insertion and removal sequence
TEST_F(UrnTest, InsertRemoveSequence) {
  uint64_t elems[] = {};
  test_urn = urn_from_array(elems, 0);

  // Insert several elements
  std::vector<std::pair<uint64_t, uint64_t>> inserted = {
      {1, 100}, {2, 200}, {3, 300}, {4, 400}};

  for (auto& pair : inserted) {
    urn_insert(test_urn, pair.first, pair.second);
  }

  EXPECT_EQ(test_urn->size, 4);

  // Remove some elements
  std::unordered_set<uint64_t> removed;
  for (int i = 0; i < 2; i++) {
    uint64_t val = urn_remove(test_urn);
    removed.insert(val);
  }

  EXPECT_EQ(test_urn->size, 2);
  EXPECT_EQ(removed.size(), 2);

  // Insert more elements
  urn_insert(test_urn, 5, 500);
  urn_insert(test_urn, 6, 600);

  EXPECT_EQ(test_urn->size, 4);
}

// Test behavioral invariants
TEST_F(UrnTest, BehavioralInvariants) {
  uint64_t elems[] = {1, 10, 2, 20, 3, 30, 4, 40, 5, 50};
  test_urn = urn_from_array(elems, 5);

  // Test that we can remove elements and they're from the original set
  std::unordered_set<uint64_t> expected = {10, 20, 30, 40, 50};
  std::unordered_set<uint64_t> removed;

  while (test_urn->size > 0) {
    uint64_t val = urn_remove(test_urn);
    EXPECT_TRUE(expected.count(val)) << "Removed unexpected value: " << val;
    removed.insert(val);
  }

  EXPECT_EQ(removed.size(), 5);
  EXPECT_EQ(removed, expected);
}

// Test memory cleanup
TEST_F(UrnTest, MemoryCleanup) {
  uint64_t elems[] = {1, 10, 2, 20, 3, 30};
  test_urn = urn_from_array(elems, 3);

  ASSERT_NE(test_urn, nullptr);

  // Free should not crash
  urn_free(test_urn);
  test_urn = nullptr;  // Prevent double-free in TearDown
}

// Test edge case: large number of insertions
TEST_F(UrnTest, LargeUrn) {
  uint64_t elems[] = {};
  test_urn = urn_from_array(elems, 0);

  const int num_elements = 100;

  // Insert many elements
  for (int i = 1; i <= num_elements; i++) {
    urn_insert(test_urn, 1, i * 10);
  }

  EXPECT_EQ(test_urn->size, num_elements);

  // Remove half the elements
  for (int i = 0; i < num_elements / 2; i++) {
    urn_remove(test_urn);
  }

  EXPECT_EQ(test_urn->size, num_elements / 2);
}
