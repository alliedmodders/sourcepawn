// vim: set sts=4 ts=8 sw=4 tw=99 et:
//
// Copyright (C) 2026 AlliedModders LLC
//
// This file is part of SourcePawn. SourcePawn is free software: you can
// redistribute it and/or modify it under the terms of the GNU General Public
// License as published by the Free Software Foundation, either version 3 of
// the License, or (at your option) any later version.
//
// You should have received a copy of the GNU General Public License along with
// SourcePawn. If not, see http://www.gnu.org/licenses/.
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
