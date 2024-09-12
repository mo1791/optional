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
inline constexpr struct nullopt_t {
    constexpr nullopt_t(int) noexcept {};
} nullopt{0};
// END

inline constexpr struct from_tuple_t {
    constexpr from_tuple_t() noexcept = default;
} from_tuple {};



template <typename T> union storage_t
{

public: // type alias 

    using value_type = T;
    using const_value_type = typename std::add_const<T>::type;
    using reference        = typename std::add_lvalue_reference<T>::type;
    using const_reference  = typename std::add_lvalue_reference<const_value_type>::type;

public: // constructors 

    constexpr storage_t(trivially_init_t) noexcept(true) {}


    template <typename U> requires std::constructible_from<T, U>
    constexpr storage_t(U&& value)
        noexcept(std::is_nothrow_constructible<T, U>::value)
    {
        std::construct_at(
            std::launder(reinterpret_cast<T*>(std::addressof(m_value))),
            std::forward<U>(value)
        );
    }

    template <typename... ARGS> requires std::constructible_from<T, ARGS...>
    constexpr storage_t(in_place_t, ARGS&&... args)
            noexcept(std::is_nothrow_constructible<T, ARGS...>::value)
    {
        std::construct_at(
            std::launder(reinterpret_cast<T*>(std::addressof(m_value))),
            std::forward<ARGS>(args)...);
    }

    template <template <typename...> class Tuple, typename... ARGS> requires std::constructible_from<T, ARGS...>
    constexpr storage_t(from_tuple_t, const Tuple<ARGS...>& tuple)
            noexcept(std::is_nothrow_constructible<T, ARGS...>::value)
    {
        [&]<auto... Indx>(std::integer_sequence<std::size_t, Indx...>) {
            ::new (static_cast<void*>(std::addressof(**this))) T{ std::get<Indx>(tuple)... };
        }( std::make_integer_sequence<std::size_t, sizeof...(ARGS)>{});
    }
        


    template <typename U>
    constexpr auto operator=(this storage_t& self, U&& value)
        noexcept(std::is_nothrow_assignable<T, U>::value) -> storage_t&
        requires(std::is_assignable<T, U>::value)
    {
        *self = std::forward<U>(value);
        return self;
    }


public: // Access the contained value 

    auto operator*(this const storage_t& self) noexcept -> const T&
    {
        return *std::launder(reinterpret_cast<const T*>(std::addressof(self.m_value)));
    }
    
    auto operator*(this storage_t& self) noexcept -> T&
    {
        return *std::launder(reinterpret_cast<T*>(std::addressof(self.m_value)));
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
    alignas(T) unsigned char m_value[sizeof(T)];
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
    constexpr optional() noexcept(true) = default;

    // Constructs an object that does not contain a value
    constexpr optional(nullopt_t) noexcept(true) : optional() {}


    // Constructs an optional object that contains a value
    // initialized as if direct-initializing (but not direct-list-initializing) an object of type T
    // with the expression std::forward<U>(value). 
    template <typename U> requires std::constructible_from<T, U>
    constexpr optional(U&& value) noexcept(std::is_nothrow_constructible<T, U>::value)
        : m_engaged(true)
        , m_store(std::forward<U>(value))
    {
    }

    template <typename U>
        requires std::conjunction<
                    std::bool_constant<std::constructible_from<T, U>>,
                    std::negation<std::bool_constant<std::convertible_to<U, T>>>
                >::value
    explicit constexpr optional(U&& value) noexcept(std::is_nothrow_constructible<T, U>::value)
        : m_engaged(true)
        , m_store(std::forward<U>(value))
    {
    }

    // Constructs an optional object that contains a value
    // initialized as if direct-initializing (but not direct-list-initializing) an object of type T
    // from the arguments std::forward<Args>(args)...
    template <typename... ARGS> requires std::constructible_from<T, ARGS...>
    constexpr optional(in_place_t indicator, ARGS&&... args)
            noexcept(std::is_nothrow_constructible<T, ARGS...>::value)
        : m_engaged(true)
        , m_store(indicator, std::forward<ARGS>(args)...)
    {
    }
    
    // Constructs an optional object that contains a value
    // initialized as if direct-initializing (but not direct-list-initializing) an object of type T
    // from tuple
    template <template <typename...> class Tuple, typename... ARGS> requires std::constructible_from<T, ARGS...>
    constexpr optional(from_tuple_t indicator, const Tuple<ARGS...>& tuple)
            noexcept(std::is_nothrow_constructible<T, ARGS...>::value)
        : m_engaged(true)
        , m_store(indicator, tuple)
    {
    }

public: 
    // Copy constructor
    constexpr optional(const optional&)
        noexcept(std::is_nothrow_copy_constructible<T>::value)
        requires(std::conjunction<std::is_copy_constructible<T>,
                                  std::is_trivially_copy_constructible<T>>::value)
    = default;

    // Copy constructor
    constexpr optional(const optional& other)
        noexcept(std::is_nothrow_copy_constructible<T>::value)
        requires(std::conjunction<std::is_copy_constructible<T>,
                                  std::negation<std::is_trivially_copy_constructible<T>>>::value)
        : optional()
    {
        if ( bool(other) )
        {
            std::construct_at(std::addressof(**this), *other);
            this->m_engaged = true;
        }
    }

    // Move constructor
    constexpr optional(optional&&)
        noexcept(std::is_nothrow_move_constructible<T>::value)
        requires(std::conjunction<std::is_move_constructible<T>,
                                  std::is_trivially_move_constructible<T>>::value)
    = default;

    // Move constructor
    constexpr optional(optional&& other)
        noexcept(std::is_nothrow_move_constructible<T>::value)
        requires(std::conjunction<std::is_move_constructible<T>,
                                  std::negation<std::is_trivially_move_constructible<T>>>::value)
        : optional()
    {
        if ( bool(other) )
        {
            std::construct_at(std::addressof(**this), std::move(*other));
            this->m_engaged = true;
        }
    }

    // Converting copy constructor ( Coercion by Member Template )
    template <typename U>
        requires std::conjunction<detail::is_not_optional<U>,
                                  std::is_constructible<T, const U&>>::value
    constexpr optional(const optional<U>& other)
        noexcept(std::is_nothrow_constructible<T, const U&>::value)
        : optional()
    {
        if ( bool(other) )
        {
            std::construct_at(std::addressof(**this), *other);
            this->m_engaged = true;
        }
    }

    // Converting move constructor ( Coercion by Member Template )
    template <typename U>
        requires std::conjunction<detail::is_not_optional<U>,
                                  std::is_constructible<T, U &&>>::value
    constexpr optional(optional<U>&& other)
        noexcept(std::is_nothrow_constructible<T, U&&>::value)
        : optional()
    {
        if ( bool(other) )
        {
            std::construct_at(std::addressof(**this), std::move(*other));
            this->m_engaged = true;
        }
    }

public:
    // the contained value is destroyed by calling its destructor
    // if *this contains a value before the call
    constexpr auto operator=(this optional& self, nullopt_t) noexcept(true) -> optional&
    {
        self.reset();
        return self;
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
    constexpr auto operator=(this optional& self, const optional& rhs)
        noexcept(std::conjunction<std::is_nothrow_copy_assignable<T>,
                                  std::is_nothrow_copy_constructible<T>>::value) -> optional&
        requires(std::conjunction<std::is_copy_assignable<T>,
                                  std::is_copy_constructible<T>,
                                  std::negation<std::is_trivially_copy_assignable<T>>>::value)
    {
        if ( bool(rhs) )
        {
            if ( bool(self) ) { *self = *rhs; }
            else
            {
                std::construct_at(std::addressof(*self), *rhs);
                self.m_engaged = true;
            }
        }
        else
        {
            if ( bool(self) ) { self.reset(); }
        }
        return self;
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
    constexpr auto operator=(this optional& self, optional&& rhs)
        noexcept(std::conjunction<std::is_nothrow_move_assignable<T>,
                                  std::is_nothrow_move_constructible<T>>::value) -> optional&
        requires(std::conjunction<std::is_move_assignable<T>,
                                  std::is_move_constructible<T>,
                                  std::negation<std::is_trivially_move_assignable<T>>>::value)
    {
        if ( bool(rhs) )
        {
            if ( bool(self) ) { *self = std::move(*rhs); }
            else
            {
                std::construct_at(std::addressof(*self), std::move(*rhs));
                self.m_engaged = true;
            }
        }
        else
        {
            if ( bool(self) )  { self.reset(); }
        }

        return self;
    }

    // Converting copy assignment ( Coercion by Member Template )
    template <typename U>
    constexpr auto operator=(this optional& self, const optional<U>& rhs)
        noexcept(std::conjunction<std::is_nothrow_assignable<T, const U&>,
                                  std::is_nothrow_constructible<T, const U&>>::value) -> optional&
        requires(std::conjunction<detail::is_not_optional<U>,
                                  std::is_assignable<T, const U&>,
                                  std::is_constructible<T, const U&>>::value)
    {
        if ( bool(rhs) )
        {
            if ( bool(self) ) { *self = *rhs; }
            else
            {
                std::construct_at(std::addressof(*self), *rhs);
                self.m_engaged = true;
            }
        }
        else
        {
            if ( bool(self) ) { self.reset(); }
        }
        return self;
    }

    // Converting move assignment ( Coercion by Member Template )
    template <typename U>
    constexpr auto operator=(this optional& self, optional<U>&& rhs)
        noexcept(std::conjunction<std::is_nothrow_assignable<T, U&&>,
                                  std::is_nothrow_constructible<T, U&&>>::value) -> optional&
        requires(std::conjunction<detail::is_not_optional<U>,
                                  std::is_assignable<T, U &&>,
                                  std::is_constructible<T, U &&>>::value)
    {
        if ( bool(rhs) )
        {
            if ( bool(self) ) { *self = std::move(*rhs); }
            else
            {
                std::construct_at(std::addressof(*self), std::move(*rhs));
                self.m_engaged = true;
            }
        }
        else
        {
            if ( bool(self) )  { self.reset(); }
        }
        return self;
    }

    // Perfect-forwarded assignment
    template <typename U>
    constexpr auto operator=(this optional& self, U&& value)
        noexcept(std::conjunction<std::is_nothrow_assignable<T, U&&>,
                                  std::is_nothrow_constructible<T, U&&>>::value) -> optional &
        requires(std::conjunction<std::is_assignable<T, U&&>,
                                  std::is_constructible<T, U&&>>::value)
    {
        if ( bool(self) ) { *self = std::forward<U>(value); }
        else
        {
            std::construct_at(std::addressof(*self), std::forward<U>(value));
            self.m_engaged = true;
        }
        return self;
    }


public: // Access contained value

    auto operator*(this const optional& self) noexcept(true) -> const T&
    {
        assert( boo(self) && "Dereferencing disengaged optional");
        return *self.m_store;
    }
    
    auto operator*(this optional& self) noexcept(true) -> T&
    {
        assert( bool(self) && "Dereferencing disengaged optional");
        return *self.m_store;
    }

    auto operator->(this const optional& self) noexcept(true) -> const T*
    {
        assert( bool(self) && "Accessing disengaged optional");
        return std::addressof(*self);
    }
    
    auto operator->(this optional& self) noexcept(true) -> T*
    {
        assert( bool(self) && "Accessing disengaged optional");
        return std::addressof(*self);
    }

public: // checks whether the object contains a value 

    constexpr operator bool(this const optional& self) noexcept(true) { return self.m_engaged; }

    constexpr bool has_value(this const optional& self) noexcept(true) { return self.m_engaged; }


public:

    // Observer the contained value
    auto value(this const optional& self) noexcept(false) -> const T&
    {
        if ( bool(self) ) { return *self; }

        throw bad_optional_access{"no value engaged!"};
    }
    // Observer the contained value
    auto value(this optional& self) noexcept(false) -> T&
    {
        if ( bool(self) ) { return *self; }

        throw bad_optional_access{"no value engaged!"};
    }

    // returns the contained value if available, another value otherwise 
    template <typename U> requires std::convertible_to<U, T>
    auto value_or(this const optional& self, U&& value) noexcept(std::is_nothrow_convertible<U, T>::value ) -> T
    {
        if ( bool(self) ) { return *self; }

        return static_cast<T>(std::forward<U>(value));
    }

    // returns the contained value if available, another value otherwise 
    template <typename U> requires std::convertible_to<U, T>
    auto value_or(this optional&& self, U&& value) noexcept(std::is_nothrow_convertible<U, T>::value) -> T
    {
        if ( bool(self) )
        {
            return std::move( *static_cast<optional&>(self) );
        }
        return static_cast<T>(std::forward<U>(value));
    }

public: 
    // assignment ( constructs the contained value in-place )
    template <typename... ARGS> requires std::constructible_from<T, ARGS...>
    void emplace(this optional& self, ARGS&& ...args) 
        noexcept(std::is_nothrow_constructible<T, ARGS...>::value)
    {
        if ( bool(self) ) { self.reset(); }

        std::construct_at(std::addressof(*self), std::forward<ARGS>(args)...);
        self.m_engaged = true;
    }

public: 
    // Swap ( exchanges the contents )
    void swap(this optional& self, optional& rhs) noexcept(std::is_nothrow_swappable<T>::value)
        requires(std::is_swappable<T>::value)
    {
        if ( bool(self) )
        {
            if ( bool(rhs) )
            {
                std::swap( *self, *rhs );
            }
            else
            {
                std::construct_at(std::addressof(*rhs), std::move(*self));
                rhs.m_engaged = true;

                self.reset();
            }
        }
        else {
            if ( bool(rhs) )
            {
                std::construct_at(std::addressof(*self), std::move(*rhs));
                self.m_engaged = true;

                rhs.reset();
            }
            else
            {
                return void();
            }
        }
    }

public:
    // destroys any contained value
    void reset(this optional& self) noexcept(std::is_nothrow_destructible<T>::value)
    {
        if ( bool(self) )
        {
            std::destroy_at(std::addressof(*self));
            self.m_engaged = false;
        }
    }


public:

    // destroys the contained value, if there is one 
    constexpr ~optional()
        noexcept(std::is_nothrow_destructible<T>::value)
        requires(std::is_trivially_destructible<T>::value)
    = default;

    // destroys the contained value, if there is one 
    ~optional()
        noexcept(std::is_nothrow_destructible<T>::value)
        requires(std::conjunction<std::negation<std::is_trivially_destructible<T>>,
                                  std::is_destructible<T>>::value)
    {
        reset();
    }


private:
    bool m_engaged { false };
    storage_t<T> m_store { trivially_init };
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

    constexpr optional() noexcept(true) = default;

    constexpr optional(nullopt_t) noexcept(true) : optional() {}

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

    constexpr auto operator=(this optional& self, const optional& rhs) noexcept(true) -> optional&
    {
        self.m_reference = rhs.m_reference;
        return self;
    }

    auto operator=(optional&&) noexcept(true) = delete;


public: // Access the contained value

    auto operator*(this const optional& self) noexcept(true) -> const T&
    {
        assert( bool(self) && "Dereferencing disengaged optional");
        return *self.m_reference;
    }
    auto operator*(this optional& self) noexcept(true) -> T&
    {
        assert( bool(self) && "Dereferencing disengaged optional");
        return *self.m_reference;
    }

    auto operator->(this const optional& self) noexcept(true) -> const T*
    {
        assert( bool(self) && "Accessing disengaged optional");
        return self.m_reference;
    }
    auto operator->(this optional& self) noexcept(true) -> T*
    {
        assert( bool(self) && "Accessing disengaged optional");
        return self.m_reference;
    }

public: // Observer the contained value

    auto value(this const optional& self) noexcept(false) -> const T&
    {
        if ( bool( self ) ) { return *self; }

        throw bad_optional_access{"no value engaged!"};
    }
    auto value(this optional& self) noexcept(false) -> T&
    {
        if ( bool( self ) ) { return *self; }

        throw bad_optional_access{"no value engaged!"};
    }

    template <typename U> requires std::convertible_to<U, T>
    auto value_or(this const optional& self, U& value)
        noexcept(std::is_nothrow_convertible<U, T>::value) -> typename std::decay<T>::type
    {
        if ( bool(self) ) { return *self; }

        return static_cast<typename std::decay<T>::type>(std::forward<U>(value));
    }

public: // assignment
    auto emplace(this optional& self, T& value) noexcept -> optional &
    {
        *self = std::addressof(value);

        return self;
    }

    auto emplace(T&&) noexcept -> optional & = delete;


public: // Swap ( exchanges the contents )
    void swap(this optional& self, optional& rhs) noexcept( true )
    {
        std::swap( *rhs, *self );
    }


public: // checks whether the object contains a value

    constexpr operator bool(this const optional& self) noexcept(true) { return ( self.m_reference != nullptr ); }

    constexpr bool has_value(this const optional& self) noexcept(true) { return ( self.m_reference != nullptr ); }

public:
    void reset(this optional& self) noexcept(true) { self.m_reference = nullptr; }

public:
    constexpr ~optional() noexcept(true) = default;

private:
   typename std::add_pointer<T>::type m_reference = nullptr;
};

template <typename T>
class optional<T&&>
{
    static_assert(sizeof(T) == 0UL, "disallowed for rvalue reference!");
};



/****** Relational operators ******/

template <typename T, typename U>
    requires std::conjunction<std::bool_constant<std::equality_comparable<T>>,
                              std::bool_constant<std::equality_comparable_with<U, T>>>::value
constexpr bool operator==(optional<T> const& lhs, optional<U> const& rhs) noexcept
{
    return ( bool(rhs) && bool(lhs) ) ? ( *lhs == *rhs ) : false;
}

template <typename T, typename U>
    requires std::conjunction<std::bool_constant<std::equality_comparable<T>>,
                              std::bool_constant<std::equality_comparable_with<U, T>>>::value
constexpr bool operator!=(optional<T> const& lhs, optional<U> const& rhs) noexcept
{
    return ( bool(rhs) && bool(lhs) ) ? ( *lhs != *rhs ) : false;
}


template <typename T, typename U>
    requires std::conjunction<std::bool_constant<std::totally_ordered<T>>,
                              std::bool_constant<std::totally_ordered_with<U, T>>>::value
constexpr bool operator<(optional<T> const& lhs, optional<U> const& rhs) noexcept
{
    return ( bool(rhs) && bool(lhs) ) ? ( *lhs < *rhs ) : false;
}

template <typename T, typename U>
    requires std::conjunction<std::bool_constant<std::totally_ordered<T>>,
                              std::bool_constant<std::totally_ordered_with<U, T>>>::value
constexpr bool operator<=(optional<T> const& lhs, optional<U> const& rhs) noexcept
{
    return ( bool(rhs) && bool(lhs) ) ? ( *lhs <= *rhs ) : false;
}


template <typename T, typename U>
    requires std::conjunction<std::bool_constant<std::totally_ordered<T>>,
                              std::bool_constant<std::totally_ordered_with<U, T>>>::value
constexpr bool operator>(optional<T> const& lhs, optional<U> const& rhs) noexcept
{
    return ( bool(rhs) && bool(lhs) ) ? ( *lhs > *rhs ) : false;
}

template <typename T, typename U>
    requires std::conjunction<std::bool_constant<std::totally_ordered<T>>,
                              std::bool_constant<std::totally_ordered_with<U, T>>>::value
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

template <typename T, typename U>
    requires std::conjunction<std::bool_constant<std::equality_comparable<T>>,
                              std::bool_constant<std::equality_comparable_with<U, T>>>::value
constexpr bool operator==(optional<T> const& lhs, typename std::type_identity<U>::type const& rhs) noexcept
{
    return bool(lhs) ? ( *lhs == rhs ) : false;
}

template <typename T, typename U>
constexpr bool operator==(T const& lhs, optional<U> const& rhs) noexcept
{
    return bool(rhs) ? ( lhs == *rhs ) : false;
}

template <typename T, typename U>
    requires std::conjunction<std::bool_constant<std::equality_comparable<T>>,
                              std::bool_constant<std::equality_comparable_with<U, T>>>::value
constexpr bool operator!=(optional<T> const& lhs, typename std::type_identity<U>::type const& rhs) noexcept
{
    return bool(lhs) ? ( *lhs != rhs ) : false;
}

template <typename T, typename U>
constexpr bool operator!=(T const& lhs, optional<U> const& rhs) noexcept
{
    return bool(rhs) ? ( lhs != *rhs ) : false;
}

template <typename T, typename U>
    requires std::conjunction<std::bool_constant<std::totally_ordered<T>>,
                              std::bool_constant<std::totally_ordered_with<U, T>>>::value
constexpr bool operator<(optional<T> const& lhs, typename std::type_identity<U>::type const& rhs) noexcept
{
    return bool(lhs) ? ( *lhs < rhs ) : false;
}

template <typename T, typename U>
constexpr bool operator<(T const& lhs, optional<U> const& rhs) noexcept
{
    return bool(rhs) ? ( lhs < *rhs ) : false;
}

template <typename T, typename U>
    requires std::conjunction<std::bool_constant<std::totally_ordered<T>>,
                              std::bool_constant<std::totally_ordered_with<U, T>>>::value
constexpr bool operator<=(optional<T> const& lhs, typename std::type_identity<U>::type const& rhs) noexcept
{
    return bool(lhs) ? ( *lhs <= rhs ) : false;
}

template <typename T, typename U>
constexpr bool operator<=(T const& lhs, optional<U> const& rhs) noexcept
{
    return bool(rhs) ? ( lhs <= *rhs ) : false;
}


template <typename T, typename U>
    requires std::conjunction<std::bool_constant<std::totally_ordered<T>>,
                              std::bool_constant<std::totally_ordered_with<U, T>>>::value
constexpr bool operator>(optional<T> const& lhs, typename std::type_identity<U>::type const& rhs) noexcept
{
    return bool(lhs) ? ( *lhs > rhs ) : false;
}

template <typename T, typename U>
constexpr bool operator>(T const& lhs, optional<U> const& rhs) noexcept
{
    return bool(rhs) ? ( lhs > *rhs ) : false;
}

template <typename T, typename U>
    requires std::conjunction<std::bool_constant<std::totally_ordered<T>>,
                              std::bool_constant<std::totally_ordered_with<U, T>>>::value
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

template <typename T, template <typename...> class Tuple, typename... ARGS>
auto make_optional(const Tuple<ARGS...>& tuple) -> optional<T>
{
    return optional<T>(from_tuple, tuple);
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