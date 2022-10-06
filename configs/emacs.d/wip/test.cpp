#include <cstdlib>
#include <iostream>
#include <ostream>
#include <sstream>
#include <limits>
#include <type_traits>

enum class Test0 {
  A = 1 << 0,
  B = 1 << 2,
  C = 1 << 3,
};

enum class Test1 {
  A = 0,
  B = 2,
  C = 3,
};

namespace ns0 {
namespace ns1 {
enum Test2 {
  A = 0,
  B = 2,
  C = 3,
};
}
} // namespace ns0

enum struct Test3 {
  A = 1 << 0,
  B = 1 << 2,
  C = 1 << 3,
};

struct Test4
{
    int a;
    int b;
};

class Test5
{
public:
    int c;
    int d;
};

namespace ns0 {
namespace ns1 {
enum class Test6 {
  A = 0,
  B = 2,
  C = 3,
};
}
} // namespace ns0

enum Test7 {
    BA = 1 << 0,
    BB = 1 << 2,
    BC = 1 << 3,
};

enum Test8 {
  A = 0,
  B = 2,
  C = 5,
};

enum class Test9 {
    A = 1 << 0,
    B = 1 << 1,
    C = 1 << 5,
    AC = Test9::A | Test9::C,
};

// Generated operators.

std::ostream& operator<<(std::ostream &os, Test0 bf)
{
    bool is_first = true;
    using UnderlyingT = typename std::underlying_type_t<Test0>;
    using UInt = typename std::make_unsigned_t<UnderlyingT>;
    UInt u = static_cast<UInt>(bf);
    const size_t nenums = 3;
    const char * enum_names[] = {
        "A",
        "B",
        "C"
    };
    UInt enum_values[] = {
        static_cast<UInt>(Test0::A),
        static_cast<UInt>(Test0::B),
        static_cast<UInt>(Test0::C)
    };
    for (size_t i = 0; i < nenums; i++)
    {
        bool is_set = u & enum_values[i];
        if (is_set && !is_first)
            os << " | ";
        if (is_set)
        {
            is_first = false;
            os << enum_names[i];
        }
    }
    return os;
}

std::ostream& operator<<(std::ostream &os, Test1 v)
{
    switch (v)
    {
    case Test1::A: os << "A"; break;
    case Test1::B: os << "B"; break;
    case Test1::C: os << "C"; break;
    }
    return os;
}


std::ostream& operator<<(std::ostream &os, ns0::ns1::Test2 v)
{
    switch (v)
    {
    case ns0::ns1::A: os << "A"; break;
    case ns0::ns1::B: os << "B"; break;
    case ns0::ns1::C: os << "C"; break;
    }
    return os;
}

std::ostream& operator<<(std::ostream &os, Test3 bf)
{
    bool is_first = true;
    using UnderlyingT = typename std::underlying_type_t<Test3>;
    using UInt = typename std::make_unsigned_t<UnderlyingT>;
    UInt u = static_cast<UInt>(bf);
    const size_t nenums = 3;
    const char * enum_names[] = {
        "A",
        "B",
        "C"
    };
    UInt enum_values[] = {
        static_cast<UInt>(Test3::A),
        static_cast<UInt>(Test3::B),
        static_cast<UInt>(Test3::C)
    };
    for (size_t i = 0; i < nenums; i++)
    {
        bool is_set = u & enum_values[i];
        if (is_set && !is_first)
            os << " | ";
        if (is_set)
        {
            is_first = false;
            os << enum_names[i];
        }
    }
    return os;
}

std::ostream& operator<<(std::ostream &os, const Test4 &v)
{
    os << "Test4{";
    os << "a=" << v.a << ", ";
    os << "b=" << v.b;
    os << "}";
    return os;
}

std::ostream& operator<<(std::ostream &os, const Test5 &v)
{
    os << "Test5{";
    os << "c=" << v.c << ", ";
    os << "d=" << v.d;
    os << "}";
    return os;
}

std::ostream& operator<<(std::ostream &os, ns0::ns1::Test6 v)
{
    switch (v)
    {
    case ns0::ns1::Test6::A: os << "A"; break;
    case ns0::ns1::Test6::B: os << "B"; break;
    case ns0::ns1::Test6::C: os << "C"; break;
    }
    return os;
}

std::ostream& operator<<(std::ostream &os, Test7 bf)
{
    bool is_first = true;
    using UnderlyingT = typename std::underlying_type_t<Test7>;
    using UInt = typename std::make_unsigned_t<UnderlyingT>;
    UInt u = static_cast<UInt>(bf);
    const size_t nenums = 3;
    const char * enum_names[] = {
        "BA",
        "BB",
        "BC"
    };
    UInt enum_values[] = {
        static_cast<UInt>(BA),
        static_cast<UInt>(BB),
        static_cast<UInt>(BC)
    };
    for (size_t i = 0; i < nenums; i++)
    {
        bool is_set = u & enum_values[i];
        if (is_set && !is_first)
            os << " | ";
        if (is_set)
        {
            is_first = false;
            os << enum_names[i];
        }
    }
    return os;
}

std::ostream& operator<<(std::ostream &os, Test8 v)
{
    switch (v)
    {
    case A: os << "A"; break;
    case B: os << "B"; break;
    case C: os << "C"; break;
    }
    return os;
}


std::ostream& operator<<(std::ostream &os, Test9 bf)
{
    bool is_first = true;
    using UnderlyingT = typename std::underlying_type_t<Test9>;
    using UInt = typename std::make_unsigned_t<UnderlyingT>;
    UInt u = static_cast<UInt>(bf);
    const size_t nenums = 3;
    const char * enum_names[] = {
        "A",
        "B",
        "C"
    };
    UInt enum_values[] = {
        static_cast<UInt>(Test9::A),
        static_cast<UInt>(Test9::B),
        static_cast<UInt>(Test9::C)
    };
    for (size_t i = 0; i < nenums; i++)
    {
        bool is_set = u & enum_values[i];
        if (is_set && !is_first)
            os << " | ";
        if (is_set)
        {
            is_first = false;
            os << enum_names[i];
        }
    }
    return os;
}


int main(void)
{
    std::cout << Test0::A << ", "
              << Test1::C << ", "
              << ns0::ns1::C << ", "
              << Test3::C << ", "
              << Test4{1, 2} << ", "
              << Test5{1, 2} << ", "
              << ns0::ns1::Test6::C << ", "
              << static_cast<Test7>(BA | BB) << ", "
              << Test8::B << ", "
              << Test9::AC
              << std::endl;

    return EXIT_SUCCESS;
}
