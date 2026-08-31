// vim: set sts=4 ts=8 sw=4 tw=99 et:
//
// SPDX-License-Identifier: BSD-3-Clause
//
// Copyright (c) 2026 AlliedModders LLC
//
#pragma once

namespace sp {

// Simple class for automatic refcounting.
template <typename T>
class Handle
{
  public:
    Handle(T* thing)
     : thing_(thing)
    {
        AddHandle();
    }

    Handle() : thing_(nullptr)
    {}

    Handle(const Handle& other)
     : thing_(other.thing_)
    {
        AddHandle();
    }

    Handle(Handle&& other)
     : thing_(other.thing_)
    {
        other.thing_ = nullptr;
    }

    ~Handle() {
        Release();
    }

    T* operator ->() const { return operator*(); }
    T* operator *() const { return thing_; }
    bool operator !() const { return !thing_; }
    operator T&() { return *thing_; }
    explicit operator bool() const { return !!thing_; }

    Handle& operator =(const Handle& other) {
        Release();
        thing_ = other.thing_;
        AddHandle();
        return *this;
    }

    Handle& operator =(Handle&& other) {
        Release();
        thing_ = other.thing_;
        other.thing_ = nullptr;
        return *this;
    }

    T* get() const { return thing_; }
    T* release() {
        T* thing = thing_;
        thing_ = nullptr;
        return thing;
    }

  private:
    void AddHandle() {
        if (thing_)
            thing_->AddRef();
    }

    void Release() {
        if (thing_)
            thing_->Release();
    }

  protected:
    T* thing_;
};

} // namespace sp
