#include <gtest/gtest.h>
#include "heap-defaults.h"

using namespace sp;

class HeapTest : public testing::Test {
  protected:
    void SetUp() override {
        ASSERT_TRUE(heap.Initialize());
    }

    HeapImpl heap;
};

TEST_F(HeapTest, Basic) {
    uint8_t* p1 = heap.Allocate(100);
    ASSERT_NE(p1, nullptr);

    uint8_t* p2 = heap.Allocate(200);
    ASSERT_NE(p2, nullptr);

    // Validate that the pointers do not overlap within the same chunk.
    ASSERT_GT(p2, p1);
    ASSERT_GE(p2, p1 + 100);
}

TEST_F(HeapTest, LargeAllocation) {
    uint8_t* p1 = heap.Allocate(kDefaultHeapChunkSize * 10);
    ASSERT_NE(p1, nullptr);
}

TEST_F(HeapTest, PositionRestore) {
    // Allocate some initial memory.
    void* p1 = heap.Allocate(1024);
    ASSERT_NE(p1, nullptr);

    HeapImpl::Position pos = heap.GetPosition();

    // Allocate more. Use a large enough size to potentially cross chunk boundaries if applicable.
    void* p2 = heap.Allocate(kDefaultHeapChunkSize * 2);
    ASSERT_NE(p2, nullptr);

    heap.RestorePosition(pos);

    // This should return same address as p2.
    void* p3 = heap.Allocate(kDefaultHeapChunkSize * 2);
    ASSERT_EQ(p2, p3);

    heap.RestorePosition(pos);

    // This should return a new address.
    void* p4 = heap.Allocate(kDefaultHeapChunkSize * 3);
#ifdef KE_32BIT
    ASSERT_NE(p2, p4);
#else
    ASSERT_EQ(p2, p4);
#endif
}

int main(int argc, char** argv) {
    testing::InitGoogleTest(&argc, argv);
    return RUN_ALL_TESTS();
}
