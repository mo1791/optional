#include "optional.hxx"
#include <format>
#include <utility>


class point
{
public:
    point(float x, float y, float z) noexcept
        : m_x(x) ,m_y(y) ,m_z(z)
    {
        std::cout << "POINT(float, float, float)\xa";
    }

    point(point const& rhs) noexcept
        : m_x(rhs.m_x) ,m_y(rhs.m_y) ,m_z(rhs.m_z)
    {
        std::cout << "POINT( point const& )\xa";
    }

    point(point&& rhs) noexcept
        : m_x( std::exchange(rhs.m_x, 0.0f) )
        , m_y( std::exchange(rhs.m_y, 0.0f) )
        , m_z( std::exchange(rhs.m_z, 0.0f) )
    {
        std::cout << "POINT( point&& )\xa";
    }

    ~point() noexcept = default;

private:
    float m_x, m_y, m_z;
};



auto main() -> int
{
    optional lhs = make_optional(148);
    optional rhs = make_optional(178);

    
    std::boolalpha(std::cout);

    std::puts("=============================================\n");

    std::cout << std::format("lhs: {}, rhs : {}\xa", *lhs, *rhs);
    std::cout << std::format("lhs({}) == rhs({}) ? {}\xa",  *lhs, *rhs, (lhs == rhs));
    std::cout << std::format("lhs({}) != rhs({}) ? {}\xa",  *lhs, *rhs, (lhs != rhs));
    std::cout << std::format("lhs({})  < rhs({}) ? {}\xa",  *lhs, *rhs, (lhs < rhs));
    std::cout << std::format("lhs({})  > rhs({}) ? {}\xa",  *lhs, *rhs, (lhs > rhs));
    std::cout << std::format("lhs({}) <= rhs({}) ? {}\xa",  *lhs, *rhs, (lhs <= rhs));
    std::cout << std::format("lhs({}) >= rhs({}) ? {}\xa",  *lhs, *rhs, (lhs >= rhs));

    std::puts("\n=============================================");
    
    
    std::puts("============lvalue construct (copy)============\n");
    point p(1.5f, 3.58f, 5.57f );

    optional<point> opt1 { p };
    std::puts("\n=============================================");

    std::puts("============rvalue construct (move)============\n");
    optional<point> opt2 { point(2.8f, 7.0f, 1.5f ) };
    std::puts("\n=============================================");
    
    std::puts("=============in-place construct==============\n");
    optional<point> opt3{ in_place, 2.8f, 7.0f, 1.5f };
    std::puts("\n=============================================");
}