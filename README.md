# C++ Optional

## Purpose
A single-header header-only library for representing optional (nullable) objects for C++



# TEST 1
```c++
#include "optional.hxx"
#include <iostream>

int main()
{
    auto opt = make_optional<int>(124.52f);

    std::cout << std::boolalpha << "bool(opt): " << bool(opt) << std::endl; 
    std::cout << "*opt: " << *opt << std::endl;


    if ( auto obj = optional<int>(nullopt) ) {
        std::cout << "success!" << std::endl;
    }
    else {
        std::cout << "failed!" << std::endl;
    }

}
```

# TEST 2
``` c++
#include "optional.hxx"
#include <format>
#include <optional>

class base
{
public: 
    constexpr base(int a, int b) noexcept(true) : m_a(a), m_b(b) {}

    constexpr virtual ~base() noexcept(true) = default;

    virtual void print() const noexcept(true)
    {
        std::cout << std::format("base( a: {}, b: {} )\n", m_a, m_b);
    }
protected:
    int m_a;
    int m_b;
};

class child final: public base
{
public:
    constexpr child(float f, int a, int b) noexcept(true) : base(a,b), m_f(f) {}

    void print() const noexcept(true)
    {
        std::cout << std::format("child( f: {:+.2f}, a: {}, b: {} )\n", m_f, m_a, m_b);
    }

public:
    constexpr ~child() noexcept(true) = default;

private:
    float m_f;
};


int main()
{
    auto child_ = child(1.25f, 58, 96);
    auto base_  = base(66,34);
    optional<base&> obj1(child_), obj2(base_);

    obj1->print(); // child( f: +1.25, a: 58, b: 96 )
    obj2->print(); // base( a: 66, b: 34 )
}
```