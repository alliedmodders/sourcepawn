// vim: set sts=2 ts=8 sw=2 tw=99 et:
// 
// Copyright (C) 2006-2015 AlliedModders LLC
// 
// This file is part of SourcePawn. SourcePawn is free software: you can
// redistribute it and/or modify it under the terms of the GNU General Public
// License as published by the Free Software Foundation, either version 3 of
// the License, or (at your option) any later version.
//
// You should have received a copy of the GNU General Public License along with
// SourcePawn. If not, see http://www.gnu.org/licenses/.
//

#include <stddef.h>

#include <functional>
#include <utility>

#include <amtl/am-bits.h>
#include <amtl/am-maybe.h>
#include <amtl/am-vector.h>

namespace sp {

class BitSet
{
 public:
  BitSet()
  {}
  explicit BitSet(size_t max_bits)
   : max_bits_(ke::Some(max_bits))
  {}
  explicit BitSet(BitSet&& other)
   : words_(std::move(other.words_)),
     max_bits_(std::move(other.max_bits_))
  {}

  bool test(uintptr_t bit) {
    size_t word = word_for_bit(bit);
    if (word >= words_.size())
      return false;
    return !!(words_[word] & (uintptr_t(1) << pos_in_word(bit)));
  }

  void set(uintptr_t bit) {
    assert(!max_bits_ || bit <= *max_bits_);
    size_t word = word_for_bit(bit);
    if (word >= words_.size())
      words_.resize(word + 1);
    words_[word] |= (uintptr_t(1) << pos_in_word(bit));
  }

  void for_each(const std::function<void(uintptr_t)>& callback) {
    for (size_t i = 0; i < words_.size(); i++) {
      uintptr_t word = words_[i];

      while (word) {
        size_t bit = ke::FindRightmostBit(word);
        word &= ~(uintptr_t(1) << bit);
        callback(i * kBitsPerWord + bit);
      }
    }
  }

  BitSet& operator =(BitSet&& other) {
    words_ = std::move(other.words_);
    max_bits_ = std::move(other.max_bits_);
    return *this;
  }

 private:
  static const size_t kBitsPerWord = sizeof(uintptr_t) * 8;

  static inline size_t word_for_bit(uintptr_t bit) {
    return bit / kBitsPerWord;
  }
  static inline size_t pos_in_word(uintptr_t bit) {
    return bit % kBitsPerWord;
  }

 private:
  std::vector<uintptr_t> words_;
  ke::Maybe<size_t> max_bits_;
};

} // namespace sp
