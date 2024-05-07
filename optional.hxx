// ---------------------------------------------
// Copyright (C) 2017 - 2018 Mohammed ELomari.
// ---------------------------------------------

// A single-header header-only library for representing optional (nullable) objects for C++

#ifndef _OPTIONAL_HXX_
#define _OPTIONAL_HXX_

#include <iostream>
#include <type_traits>
#include <utility>
#include <cassert>

template <typename> class optional;

namespace detail {

template <typename T>
struct is_not_optional : public std::true_type {};

template <typename T>
struct is_not_optional<::optional<T>> : public std::false_type {};

}  // namespace detail



// class bad_optional_access 
struct bad_optional_access : public std::logic_error
{
public:
    explicit bad_optional_access(const std::string& what_arg) : std::logic_error{what_arg} {}
    explicit bad_optional_access(const char* what_arg) : std::logic_error{what_arg} {}
};
// END 


// trivially construction 
inline constexpr struct trivially_init_t { 
    constexpr trivially_init_t() noexcept = default;
} trivially_init{};
// END 

// In-place construction 
inline constexpr struct in_place_t {
    constexpr in_place_t() noexcept = default;
} in_place{};
// END


// Disengaged state indicator
struct nullopt_t {
    struct init {};
    constexpr nullopt_t(init) noexcept {};
};

inline constexpr nullopt_t nullopt{nullopt_t::init{}};
// END



template <typename T> union storage_t
{

public: // type alias 

    using value_type = T;
    using const_value_type = typename std::add_const<T>::type;
    using reference        = typename std::add_lvalue_reference<T>::type;
    using const_reference  = typename std::add_lvalue_reference<const_value_type>::type;

public: // constructors 

    constexpr storage_t(trivially_init_t) noexcept(true) {}

    template <typename... ARGS>
    constexpr storage_t(ARGS&&... args)
            noexcept(std::is_nothrow_constructible<T, ARGS...>::value)
            requires(std::is_constructible<T, ARGS...>::value)
    {
        std::construct_at(
            std::launder(reinterpret_cast<T*>(std::addressof(m_value))),
            std::forward<ARGS>(args)...);
    }

    template <typename U>
    constexpr auto operator=(U&& value)
        noexcept(std::is_nothrow_assignable<T, U>::value) -> storage_t&
        requires(std::is_assignable<T, U>::value)
    {
        **this = std::forward<U>(value);
        return *this;
    }


public: // Access the contained value 

    auto operator*() const noexcept -> const T&
    {
        return *std::launder(reinterpret_cast<const T*>(std::addressof(m_value)));
    }
    
    auto operator*() noexcept -> T&
    {
        return *std::launder(reinterpret_cast<T*>(std::addressof(m_value)));
    }

public: // ddestructors

    constexpr ~storage_t() noexcept(std::is_nothrow_destructible<T>::value)
        requires(std::is_trivially_destructible<T>::value)
    = default;

    constexpr ~storage_t() noexcept(std::is_nothrow_destructible<T>::value)
        requires(std::negation<std::is_trivially_destructible<T>>::value)
    {
    }

private:
    alignas(T) std::byte m_value[sizeof(T)];
};



// optional for objects types 
template <typename T> class optional
{
    static_assert( not std::is_same<typename std::decay<T>::type, nullopt_t>::value,  "bad T" );
    static_assert( not std::is_same<typename std::decay<T>::type, in_place_t>::value, "bad T" );

public: //  type alias

    using value_type       = T;
    using const_value_type = typename std::add_const<T>::type;
    using reference        = typename std::add_lvalue_reference<T>::type;
    using const_reference  = typename std::add_lvalue_reference<const_value_type>::type;
    using pointer          = typename std::add_pointer<T>::type;
    using const_pointer    = typename std::add_pointer<const_value_type>::type;


public:

    // Constructs an object that does not contain a value
    constexpr optional() noexcept(true)
        : m_engaged(false), m_store(trivially_init)
    { }

    // Constructs an object that does not contain a value
    constexpr optional(nullopt_t) noexcept(true) : optional() {}


    // Constructs an optional object that contains a value
    // initialized as if direct-initializing (but not direct-list-initializing) an object of type T
    // with the expression std::forward<U>(value). 
    template <typename U>
    constexpr optional(U&& value) noexcept(std::is_nothrow_constructible<T, U>::value)
            requires(std::conjunction<std::is_constructible<T, U>,
                                      std::is_convertible<U, T>>::value)
        : m_engaged(true)
        , m_store(std::forward<U>(value))
    {
    }

    template <typename U>
    explicit constexpr optional(U&& value) noexcept(std::is_nothrow_constructible<T, U>::value)
        requires(std::conjunction<std::is_constructible<T, U>,
                                  std::negation<std::is_convertible<U, T>>>::value)
        : m_engaged(true)
        , m_store(std::forward<U>(value))
    {
    }

    // Constructs an optional object that contains a value
    // initialized as if direct-initializing (but not direct-list-initializing) an object of type T
    // from the arguments std::forward<Args>(args)...
    template <typename... ARGS>
    constexpr optional(in_place_t, ARGS&&... args)
            noexcept(std::is_nothrow_constructible<T, ARGS...>::value)
            requires(std::is_constructible<T, ARGS...>::value)
        : m_engaged(true)
        , m_store(std::forward<ARGS>(args)...)
    {
    }

public: 
    // Copy constructor
    constexpr optional(const optional&)
        noexcept(std::is_nothrow_copy_constructible<T>::value)
        requires(std::conjunction<
                    std::is_copy_constructible<T>,
                    std::is_trivially_copy_constructible<T>>::value)
    = default;
    // Copy constructor
    constexpr optional(const optional& other)
        noexcept(std::is_nothrow_copy_constructible<T>::value)
        requires(std::conjunction<
                 std::is_copy_constructible<T>,
                 std::negation<std::is_trivially_copy_constructible<T>>>::value)
        : optional()
    {
        if (other.m_engaged) {
            std::construct_at(std::addressof(*m_store), *other);
            m_engaged = true;
        }
    }

    // Move constructor
    constexpr optional(optional&&)
        noexcept(std::is_nothrow_move_constructible<T>::value)
        requires(std::conjunction<
                    std::is_move_constructible<T>,
                    std::is_trivially_move_constructible<T>>::value)
    = default;

    // Move constructor
    constexpr optional(optional&& other) noexcept(
        std::is_nothrow_move_constructible<T>::value)
        requires(std::conjunction<
                 std::is_move_constructible<T>,
                 std::negation<std::is_trivially_move_constructible<T>>>::value)
        : optional()
    {
        if (other.m_engaged) {
            std::construct_at(std::addressof(*m_store), std::move(*other));
            m_engaged = true;
        }
    }

    // Converting copy constructor ( Coercion by Member Template )
    template <typename U>
    constexpr optional(const optional<U>& other) noexcept(
        std::is_nothrow_constructible<T, const U&>::value)
        requires(std::conjunction<detail::is_not_optional<U>,
                                  std::is_constructible<T, const U&>>::value)
        : optional()
    {
        if (other.m_engaged) {
            std::construct_at(std::addressof(*m_store), *other);
            m_engaged = true;
        }
    }

    // Converting move constructor ( Coercion by Member Template )
    template <typename U>
    constexpr optional(optional<U>&& other) noexcept(
        std::is_nothrow_constructible<T, U&&>::value)
        requires(std::conjunction<detail::is_not_optional<U>,
                                  std::is_constructible<T, U &&>>::value)
        : optional()
    {
        if (other.m_engaged) {
            std::construct_at(std::addressof(*m_store), std::move(*other));
            m_engaged = true;
        }
    }

public:
    // the contained value is destroyed by calling its destructor
    // if *this contains a value before the call
    constexpr auto operator=(nullopt_t) noexcept(true) -> optional&
    {
        reset();
        return *this;
    }

    // Assigns the state of other to *this ( copy )
    constexpr auto operator=(const optional&)
        noexcept(std::conjunction<std::is_nothrow_copy_assignable<T>,
                         std::is_nothrow_copy_constructible<T>>::value) -> optional&
        requires(std::conjunction<std::is_copy_assignable<T>,
                                  std::is_copy_constructible<T>,
                                  std::is_trivially_copy_assignable<T>>::value)
    = default;

    // copy assigns the state of other to *this
    constexpr auto operator=(const optional& rhs)
        noexcept(std::conjunction<std::is_nothrow_copy_assignable<T>,
                         std::is_nothrow_copy_constructible<T>>::value) -> optional&
        requires(std::conjunction<
                 std::is_copy_assignable<T>, std::is_copy_constructible<T>,
                 std::negation<std::is_trivially_copy_assignable<T>>>::value)
    {
        if (bool(rhs))
        {
            if (m_engaged) {
                m_store = *rhs;
            }
            else {
                std::construct_at(std::addressof(*m_store), *rhs);
                m_engaged = true;
            }
        }
        else {
            if (m_engaged) reset();
        }
        return *this;
    }

    // move assigns the state of other to *this
    constexpr auto operator=(optional&&)
        noexcept(std::conjunction<std::is_nothrow_move_assignable<T>,
                         std::is_nothrow_move_constructible<T>>::value) -> optional&
        requires(std::conjunction<std::is_move_assignable<T>,
                                  std::is_move_constructible<T>,
                                  std::is_trivially_move_assignable<T>>::value)
    = default;

    // Assigns the state of other to *this ( move )
    constexpr auto operator=(optional&& rhs)
        noexcept(std::conjunction<std::is_nothrow_move_assignable<T>,
                         std::is_nothrow_move_constructible<T>>::value) -> optional&
        requires(std::conjunction<
                 std::is_move_assignable<T>, std::is_move_constructible<T>,
                 std::negation<std::is_trivially_move_assignable<T>>>::value)
    {
        if (bool(rhs))
        {
            if (m_engaged) {
                m_store = std::move(*rhs);
            }
            else {
                std::construct_at(std::addressof(*m_store), std::move(*rhs));
                m_engaged = true;
            }
        }
        else {
            if (m_engaged) reset();
        }

        return *this;
    }

    // Converting copy assignment ( Coercion by Member Template )
    template <typename U>
    constexpr auto operator=(const optional<U>& rhs)
        noexcept(std::conjunction<std::is_nothrow_assignable<T, const U&>,
                         std::is_nothrow_constructible<T, const U&>>::value) -> optional&
        requires(std::conjunction<detail::is_not_optional<U>,
                                  std::is_assignable<T, const U&>,
                                  std::is_constructible<T, const U&>>::value)
    {
        if (bool(rhs))
        {
            if (m_engaged) {
                m_store = *rhs;
            }
            else {
                std::construct_at(std::addressof(*m_store), *rhs);
                m_engaged = true;
            }
        }
        else {
            if (m_engaged) reset();
        }
        return *this;
    }

    // Converting move assignment ( Coercion by Member Template )
    template <typename U>
    constexpr auto operator=(optional<U>&& rhs)
        noexcept(std::conjunction<std::is_nothrow_assignable<T, U&&>,
                         std::is_nothrow_constructible<T, U&&>>::value) -> optional&
        requires(std::conjunction<detail::is_not_optional<U>,
                                  std::is_assignable<T, U &&>,
                                  std::is_constructible<T, U &&>>::value)
    {
        if (bool(rhs))
        {
            if (m_engaged) {
                m_store = std::move(*rhs);
            }
            else {
                std::construct_at(std::addressof(*m_store), std::move(*rhs));
                m_engaged = true;
            }
        }
        else {
            if (m_engaged) reset();
        }

        return *this;
    }

    // Perfect-forwarded assignment
    template <typename U>
    constexpr auto operator=(U&& value)
        noexcept(std::conjunction<std::is_nothrow_assignable<T, U&&>,
                    std::is_nothrow_constructible<T, U&&>>::value) -> optional &
        requires(std::conjunction<std::is_assignable<T, U&&>,
                    std::is_constructible<T, U&&>>::value)
    {
        if ( m_engaged )
        {
            m_store = std::forward<U>(value);
        }
        else {
            std::construct_at(std::addressof(*m_store), std::forward<U>(value));
            m_engaged = true;
        }
        return *this;
    }


public: // Access contained value

    auto operator*() const noexcept(true) -> const T&
    {
        assert(m_engaged && "Dereferencing disengaged optional");
        return *m_store;
    }
    
    auto operator*() noexcept(true) -> T&
    {
        assert(m_engaged && "Dereferencing disengaged optional");
        return *m_store;
    }

    auto operator->() const noexcept(true) -> const T*
    {
        assert(m_engaged && "Accessing disengaged optional");
        return std::addressof(*m_store);
    }
    
    auto operator->() noexcept(true) -> T*
    {
        assert(m_engaged && "Accessing disengaged optional");
        return std::addressof(*m_store);
    }

public: // checks whether the object contains a value 

    constexpr operator bool() const noexcept(true) { return m_engaged; }

    constexpr bool has_value() const noexcept(true) { return m_engaged; }


public:

    // Observer the contained value
    auto value() const noexcept(false) -> const T&
    {
        return bool(*this)  ? *m_store : throw bad_optional_access{"no value engaged!"};
    }
    // Observer the contained value
    auto value() noexcept(false) -> T&
    {
        return bool(*this)  ? *m_store : throw bad_optional_access{"no value engaged!"};
    }

    // returns the contained value if available, another value otherwise 
    template <typename U>
    auto value_or(U&& value) const&  noexcept(std::is_nothrow_convertible<U, T>::value ) -> T
        requires( std::is_convertible<U, T>::value )
    {
        return bool(*this) ? *m_store : static_cast<T>(std::forward<U>(value));
    }

    // returns the contained value if available, another value otherwise 
    template <typename U>
    auto value_or(U&& value) && noexcept(std::is_nothrow_convertible<U, T>::value) -> T
        requires( std::is_convertible<U, T>::value )
    {
        return bool(*this)  ? std::move( *static_cast<optional&>(*this).m_store)
                            : static_cast<T>(std::forward<U>(value));
    }

public: 
    // assignment ( constructs the contained value in-place )
    template <typename... ARGS>
    void emplace(ARGS&& ...args) 
        noexcept(std::is_nothrow_constructible<T, ARGS...>::value)
        requires(std::is_constructible<T, ARGS...>::value)
    {
        if ( m_engaged ) reset();

        std::construct_at(std::addressof(*m_store), std::forward<ARGS>(args)...);
        m_engaged = true;
    }

public: 
    // Swap ( exchanges the contents )
    void swap(optional& rhs) noexcept(std::is_nothrow_swappable<T>::value)
        requires(std::is_swappable<T>::value)
    {
        if ( m_engaged )
        {
            if ( rhs.m_engaged ) {
                std::swap( *m_store, *rhs );
            }
            else {
                std::construct_at(std::addressof(*rhs), std::move(*m_store));
                rhs.m_engaged = true;

                reset();
            }
        }
        else {
            if ( rhs.m_engaged ) {
                std::construct_at(std::addressof(*m_store), std::move(*rhs));
                m_engaged = true;

                rhs.reset();
            }
            else {
                return void();
            }
        }
    }

public:
    // destroys any contained value
    void reset() noexcept(std::is_nothrow_destructible<T>::value)
    {
        if (m_engaged)
            std::destroy_at(std::addressof(*m_store));

        m_engaged = false;
    }


public:

    // destroys the contained value, if there is one 
    constexpr ~optional() noexcept(std::is_nothrow_destructible<T>::value)
        requires(std::is_trivially_destructible<T>::value)
    = default;

    // destroys the contained value, if there is one 
    ~optional() noexcept(std::is_nothrow_destructible<T>::value)
        requires(
            std::conjunction<std::negation<std::is_trivially_destructible<T>>,
                             std::is_destructible<T>>::value)
    {
        reset();
    }


private:
    bool m_engaged;
    storage_t<T> m_store;
};


template <typename T> class optional<T&>
{

    static_assert( not std::is_same<typename std::decay<T>::type, nullopt_t>::value,  "bad T" );
    static_assert( not std::is_same<typename std::decay<T>::type, in_place_t>::value, "bad T" );

private: // type alias

    using const_t         = typename std::add_const<T>::type;   
    using pointer         = typename std::add_pointer<T>::type;
    using const_pointer   = typename std::add_pointer<const_t>::type;
    using reference       = typename std::add_lvalue_reference<T>::type;
    using const_reference = typename std::add_lvalue_reference<const_t>::type;


public: // constructors

    constexpr optional() noexcept(true) : m_reference(nullptr) {}

    constexpr optional(nullopt_t) noexcept(true) : m_reference(nullptr) {}

    constexpr optional(T& value) noexcept(true)
        : m_reference(std::addressof(value))
    {
    }

    constexpr optional(in_place_t, T& value) noexcept(true)
        : m_reference(std::addressof(value))
    {
    }

    optional(T&&) noexcept(true) = delete;

    optional(in_place_t, T&&) noexcept(true) = delete;


public: // copy constructor
    
    constexpr optional(const optional& other) noexcept(true)
        : m_reference(other.m_reference)
    {
    }

    constexpr optional(optional&&) noexcept(true) = delete;

public: // assignment

    constexpr auto operator=(const optional& rhs) noexcept(true) -> optional&
    {
        m_reference = rhs.m_reference;
        return *this;
    }

    auto operator=(optional&&) noexcept(true) = delete;


public: // Access the contained value

    auto operator*() const noexcept(true) -> const T&
    {
        assert(m_reference && "Dereferencing disengaged optional");
        return *m_reference;
    }
    auto operator*() noexcept(true) -> T&
    {
        assert(m_reference && "Dereferencing disengaged optional");
        return *m_reference;
    }

    auto operator->() const noexcept(true) -> const T*
    {
        assert(m_reference && "Accessing disengaged optional");
        return m_reference;
    }
    auto operator->() noexcept(true) -> T*
    {
        assert(m_reference && "Accessing disengaged optional");
        return m_reference;
    }

public: // Observer the contained value

    auto value() const noexcept(false) -> const T&
    {
        return bool(*this)  ? *m_reference : throw bad_optional_access{"no value engaged!"};
    }
    auto value() noexcept(false) -> T&
    {
        return bool(*this)  ? *m_reference : throw bad_optional_access{"no value engaged!"};
    }

    template <typename U>
    auto value_or(U& value) const 
        noexcept(std::is_nothrow_convertible<U, 
                    typename std::decay<T>::type>::value) -> typename std::decay<T>::type
        requires(std::is_convertible<U, typename std::decay<T>::type>::value)
    {
        return bool(*this)  ? *m_reference
                            : static_cast<typename std::decay<T>::type>(std::forward<U>(value));
    }

public: // assignment
    auto emplace(T& value) noexcept -> optional &
    {
        m_reference = std::addressof(value);

        return *this;
    }

    auto emplace(T&&) noexcept -> optional & = delete;


public: // Swap ( exchanges the contents )
    void swap(optional& rhs) noexcept( true )
    {
        std::swap( rhs.m_reference, m_reference );
    }


public: // checks whether the object contains a value

    constexpr operator bool() const noexcept(true) { return ( m_reference != nullptr ); }

    constexpr bool has_value() const noexcept(true) { return ( m_reference != nullptr ); }

public:
    void reset() noexcept(true) { m_reference = nullptr; }

public:
    constexpr ~optional() noexcept(true) = default;

private:
   typename std::add_pointer<T>::type m_reference;
};

template <typename T>
class optional<T&&>
{
    static_assert(sizeof(T) == 0UL, "disallowed for rvalue reference!");
};



/****** Relational operators ******/

template <std::equality_comparable T, std::equality_comparable_with<T> U>
constexpr bool operator==(optional<T> const& lhs, optional<U> const& rhs) noexcept
{
    return ( bool(rhs) && bool(lhs) ) ? ( *lhs == *rhs ) : false;
}

template <std::equality_comparable T, std::equality_comparable_with<T> U>
constexpr bool operator!=(optional<T> const& lhs, optional<U> const& rhs) noexcept
{
    return ( bool(rhs) && bool(lhs) ) ? ( *lhs != *rhs ) : false;
}


template <std::totally_ordered T, std::totally_ordered_with<T> U>
constexpr bool operator<(optional<T> const& lhs, optional<U> const& rhs) noexcept
{
    return ( bool(rhs) && bool(lhs) ) ? ( *lhs < *rhs ) : false;
}

template <std::totally_ordered T, std::totally_ordered_with<T> U>
constexpr bool operator<=(optional<T> const& lhs, optional<U> const& rhs) noexcept
{
    return ( bool(rhs) && bool(lhs) ) ? ( *lhs <= *rhs ) : false;
}


template <std::totally_ordered T, std::totally_ordered_with<T> U>
constexpr bool operator>(optional<T> const& lhs, optional<U> const& rhs) noexcept
{
    return ( bool(rhs) && bool(lhs) ) ? ( *lhs > *rhs ) : false;
}

template <std::totally_ordered T, std::totally_ordered_with<T> U>
constexpr bool operator>=(optional<T> const& lhs, optional<U> const& rhs) noexcept
{
    return ( bool(rhs) && bool(lhs) ) ? ( *lhs >= *rhs ) : false;
}



/***** Comparison with nullopt ******/

template <typename T>
constexpr bool operator==(optional<T> const& lhs, nullopt_t) noexcept { return not( bool(lhs) ); }

template <typename T>
constexpr bool operator==(nullopt_t, optional<T> const& rhs) noexcept { return not( bool(rhs) ); }

template <typename T>
constexpr bool operator!=(optional<T> const& lhs, nullopt_t) noexcept { return bool(lhs); }

template <typename T>
constexpr bool operator!=(nullopt_t, optional<T> const& rhs) noexcept { return bool(rhs); }

template <typename T>
constexpr bool operator<(optional<T> const&, nullopt_t) noexcept { return false; }

template <typename T>
constexpr bool operator<(nullopt_t, optional<T> const& rhs) noexcept { return bool(rhs); }

template <typename T>
constexpr bool operator<=(optional<T> const& lhs, nullopt_t) noexcept { return not( bool(lhs) ); }

template <typename T>
constexpr bool operator<=(nullopt_t, optional<T> const&) noexcept { return true; }

template <typename T>
constexpr bool operator>(optional<T> const& lhs, nullopt_t) noexcept { return bool(lhs); }

template <typename T>
constexpr bool operator>(nullopt_t, optional<T> const&) noexcept { return false; }

template <typename T>
constexpr bool operator>=(optional<T> const&, nullopt_t) noexcept { return true; }

template <typename T>
constexpr bool operator>=(nullopt_t, optional<T> const& rhs) noexcept { return not( bool(rhs) ); }



/** Comparison with  U **/

template <std::equality_comparable T, std::equality_comparable_with<T> U>
constexpr bool operator==(optional<T> const& lhs, typename std::type_identity<U>::type const& rhs) noexcept
{
    return bool(lhs) ? ( *lhs == rhs ) : false;
}

template <typename T, typename U>
constexpr bool operator==(T const& lhs, optional<U> const& rhs) noexcept
{
    return bool(rhs) ? ( lhs == *rhs ) : false;
}

template <std::equality_comparable T, std::equality_comparable_with<T> U>
constexpr bool operator!=(optional<T> const& lhs, typename std::type_identity<U>::type const& rhs) noexcept
{
    return bool(lhs) ? ( *lhs != rhs ) : false;
}

template <typename T, typename U>
constexpr bool operator!=(T const& lhs, optional<U> const& rhs) noexcept
{
    return bool(rhs) ? ( lhs != *rhs ) : false;
}

template <std::totally_ordered T, std::totally_ordered_with<T> U>
constexpr bool operator<(optional<T> const& lhs, typename std::type_identity<U>::type const& rhs) noexcept
{
    return bool(lhs) ? ( *lhs < rhs ) : false;
}

template <typename T, typename U>
constexpr bool operator<(T const& lhs, optional<U> const& rhs) noexcept
{
    return bool(rhs) ? ( lhs < *rhs ) : false;
}

template <std::totally_ordered T, std::totally_ordered_with<T> U>
constexpr bool operator<=(optional<T> const& lhs, typename std::type_identity<U>::type const& rhs) noexcept
{
    return bool(lhs) ? ( *lhs <= rhs ) : false;
}

template <typename T, typename U>
constexpr bool operator<=(T const& lhs, optional<U> const& rhs) noexcept
{
    return bool(rhs) ? ( lhs <= *rhs ) : false;
}


template <std::totally_ordered T, std::totally_ordered_with<T> U>
constexpr bool operator>(optional<T> const& lhs, typename std::type_identity<U>::type const& rhs) noexcept
{
    return bool(lhs) ? ( *lhs > rhs ) : false;
}

template <typename T, typename U>
constexpr bool operator>(T const& lhs, optional<U> const& rhs) noexcept
{
    return bool(rhs) ? ( lhs > *rhs ) : false;
}

template <std::totally_ordered T, std::totally_ordered_with<T> U>
constexpr bool operator>=(optional<T> const& lhs, typename std::type_identity<U>::type const& rhs) noexcept
{
    return bool(lhs) ? ( *lhs >= rhs ) : false;
}

template <typename T, typename U>
constexpr bool operator>=(T const& lhs, optional<U> const& rhs) noexcept
{
    return bool(rhs) ? ( lhs >= *rhs ) : false;
}



/** Specialized algorithms **/
template <typename T>
void swap(optional<T>& lhs, optional<T>& rhs) noexcept(noexcept(lhs.swap(rhs)))
{
    lhs.swap(rhs);
}

template <typename T>
auto make_optional(T&& value) -> optional<typename std::decay<T>::type>
{
    return optional<typename std::decay<T>::type>( std::forward<T>(value) );
}

template <typename T, typename... ARGS>
auto make_optional(ARGS&& ...args) -> optional<T>
{
    return optional<T>(in_place, std::forward<ARGS>(args)...);
}

/** END **/



namespace std
{
    template <typename T> 
    struct hash<::optional<T>>
    {
    public:
        using result_type   = typename hash<T>::result_type;
        using argument_type = ::optional<T>;
    
    public:    
        constexpr auto operator()(argument_type const& arg) const -> result_type
        {
            return arg ? std::hash<T>{}(*arg) : result_type{};
        }
    };
      
    template <typename T> 
    struct hash<::optional<T&>>
    {
        using result_type   = typename hash<T>::result_type;
        using argument_type = ::optional<T&>;
        
        constexpr auto operator()(argument_type const& arg) const -> result_type
        {
            return arg ? std::hash<T>{}(*arg) : result_type{};
        }
    };
}

#endif
