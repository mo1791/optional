// ---------------------------------------------
// Copyright (C) 2017 - 2018 Mohammed ELomari.
// ---------------------------------------------

// A single-header header-only library for representing optional (nullable) objects for C++

#ifndef _OPTIONAL_HXX_
#define _OPTIONAL_HXX_

#include <cassert>
#include <iostream>
#include <memory>
#include <type_traits>
#include <utility>


template <typename> class optional;

namespace detail
{
    template <typename T>
    struct is_not_optional : public std::true_type {};

    template <typename T>
    struct is_not_optional<::optional<T>> : public std::false_type {};
} // namespace detail

// class bad_optional_access
struct bad_optional_access : public std::logic_error
{
public:
    explicit bad_optional_access (const std::string &what_arg)
        : std::logic_error{ what_arg }
    {}
    
    explicit bad_optional_access (const char *what_arg)
        : std::logic_error{ what_arg }
    {}
};
// END

// In-place construction
inline constexpr struct in_place_t
{
    constexpr in_place_t (int) noexcept {}
} in_place{ 0 };
// END

// Disengaged state indicator
inline constexpr struct nullopt_t
{
    constexpr nullopt_t (int) noexcept {};
} nullopt{ 0 };
// END

// From Tuple Construction
inline constexpr struct from_tuple_t
{
    constexpr from_tuple_t (int) noexcept {}
} from_tuple{ 0 };
// END

// optional for objects types
template <typename T> class optional
{
    static_assert(!std::same_as<std::decay_t<T>, nullopt_t>, "bad T");
    static_assert(!std::same_as<std::decay_t<T>, in_place_t>, "bad T");

private:
    union storage_t;

public: //  type alias
    using value_type = T;
    using reference        = T&;
    using const_reference  = const T&;
    using pointer          = T*;
    using const_pointer    = const T*;

public:
    // Constructs an object that does not contain a value
    constexpr optional() noexcept (true)
        : m_engaged{ false }, m_store{}
    {}

    // Constructs an object that does not contain a value
    constexpr optional(nullopt_t) noexcept
        : optional{}
    {}

    // Constructs an optional object that contains a value
    // initialized as if direct-initializing (but not direct-list-initializing)
    // an object of type T with the expression std::forward<U>(value).
    template <typename U> requires std::constructible_from<T, U>
    constexpr optional(U &&value) noexcept (std::is_nothrow_constructible<T, U>::value)
        : m_engaged{true}, m_store{ std::forward<U>(value) }
    {
    }

    template <typename U>
        requires( std::constructible_from<T, U> &&
                 !std::convertible_to<U, T> )
    explicit constexpr optional(U &&value)
        noexcept(std::is_nothrow_constructible<T, U>::value)
        : m_engaged{true}, m_store{std::forward<U> (value)}
    {
    }

    // Constructs an optional object that contains a value
    // initialized as if direct-initializing (but not direct-list-initializing)
    // an object of type T from the arguments std::forward<Args>(args)...
    template <typename... ARGS>
        requires std::constructible_from<T, ARGS...>
    constexpr optional(in_place_t indicator, ARGS &&...args)
        noexcept (std::is_nothrow_constructible<T, ARGS...>::value)
        : m_engaged{true}, m_store{indicator, std::forward<ARGS> (args)...}
    {}

    // Constructs an optional object that contains a value
    // initialized as if direct-initializing (but not direct-list-initializing)
    // an object of type T from tuple
    template <template <typename...> class Tuple, typename... ARGS>
        requires std::constructible_from<T, ARGS...>
    constexpr optional(from_tuple_t indicator, const Tuple<ARGS...> &tuple)
        noexcept(std::is_nothrow_constructible<T, ARGS...>::value)
        : m_engaged{true}, m_store{indicator, tuple}
    {}

public:
    // Copy constructor
    constexpr optional (const optional &)
        noexcept(std::is_nothrow_copy_constructible<T>::value)
        requires(std::conjunction<
                    std::is_copy_constructible<T>,
                    std::is_trivially_copy_constructible<T>>::value)
    = default;

    // Copy constructor
    constexpr optional(const optional &other)
        noexcept(std::is_nothrow_copy_constructible<T>::value)
        requires(std::conjunction<
                    std::is_copy_constructible<T>,
                    std::negation<std::is_trivially_copy_constructible<T>>>::value)
        : optional{}
    {
        if (bool(other))
            {
                std::construct_at(std::to_address(*this), *other);
                this->m_engaged = true;
            }
    }

    // Move constructor
    constexpr optional(optional &&)
        noexcept(std::is_nothrow_move_constructible<T>::value)
        requires(std::conjunction<
                    std::is_move_constructible<T>,
                    std::is_trivially_move_constructible<T>>::value)
    = default;

    // Move constructor
    constexpr optional(optional &&other)
        noexcept(std::is_nothrow_move_constructible<T>::value)
        requires(
            std::conjunction<
                std::is_move_constructible<T>,
                std::negation<std::is_trivially_move_constructible<T>>>::value)
        : optional{}
    {
        if (bool(other))
            {
                std::construct_at(std::to_address(*this), std::move(*other));
                this->m_engaged = true;
            }
    }

    // Converting copy constructor ( Coercion by Member Template )
    template <typename U>
        requires std::conjunction<detail::is_not_optional<U>,
                                  std::is_constructible<T, const U &>>::value
    constexpr optional(const optional<U> &other)
        noexcept(std::is_nothrow_constructible<T, const U &>::value)
        : optional{}
    {
        if (bool(other))
            {
                std::construct_at(std::to_address(*this), *other);
                this->m_engaged = true;
            }
    }

    // Converting move constructor ( Coercion by Member Template )
    template <typename U>
        requires std::conjunction<detail::is_not_optional<U>,
                                  std::is_constructible<T, U &&>>::value
    constexpr optional(optional<U> &&other)
        noexcept(std::is_nothrow_constructible<T, U &&>::value)
        : optional{}
    {
        if (bool(other))
            {
                std::construct_at(std::to_address(*this), std::move(*other));
                this->m_engaged = true;
            }
    }

public:
    // the contained value is destroyed by calling its destructor
    // if *this contains a value before the call
    constexpr optional&
    operator=(nullopt_t) noexcept
    {
        this->reset();
        return *this;
    }

    // Assigns the state of other to *this ( copy )
    constexpr optional&
    operator=(const optional &) noexcept(std::conjunction<
                                            std::is_nothrow_copy_assignable<T>,
                                            std::is_nothrow_copy_constructible<T>>::value)
        requires(std::conjunction<
                    std::is_copy_assignable<T>,
                    std::is_copy_constructible<T>,
                    std::is_trivially_copy_assignable<T>>::value)
    = default;

    // copy assigns the state of other to *this
    constexpr optional&
    operator=(const optional &rhs)
        noexcept(std::conjunction<
                    std::is_nothrow_copy_assignable<T>,
                    std::is_nothrow_copy_constructible<T>>::value)
        requires(std::conjunction<
                    std::is_copy_assignable<T>,
                    std::is_copy_constructible<T>,
                    std::negation<std::is_trivially_copy_assignable<T>>>::value)
    {
        if (bool(rhs))
            {
                if (bool(*this))
                    {
                        **this = *rhs;
                    }
                else
                    {
                        std::construct_at(std::to_address(*this), *rhs);
                        this->m_engaged = true;
                    }
            }
        else
            {
                if (bool(*this))
                    {
                        this->reset ();
                    }
            }
        return *this;
    }

    // move assigns the state of other to *this
    constexpr optional&
    operator=(optional &&) noexcept(std::conjunction<std::is_nothrow_move_assignable<T>,
                                                     std::is_nothrow_move_constructible<T>>::value)
        requires (std::conjunction<
                        std::is_move_assignable<T>,
                        std::is_move_constructible<T>,
                        std::is_trivially_move_assignable<T>>::value)
    = default;

    // Assigns the state of other to *this ( move )
    constexpr optional&
    operator=(optional &&rhs)
        noexcept(std::conjunction<
                    std::is_nothrow_move_assignable<T>,
                    std::is_nothrow_move_constructible<T>>::value)
        requires(std::conjunction<
                    std::is_move_assignable<T>,
                    std::is_move_constructible<T>,
                    std::negation<std::is_trivially_move_assignable<T>>>::value)
    {
        if (bool(rhs))
            {
                if (bool(*this))
                    {
                        **this = std::move(*rhs);
                    }
                else
                    {
                        std::construct_at(std::to_address(*this), std::move(*rhs));
                        this->m_engaged = true;
                    }
            }
        else
            {
                if (bool(*this))
                    {
                        this->reset ();
                    }
            }

        return *this;
    }

  // Converting copy assignment ( Coercion by Member Template )
    template <typename U>
    constexpr optional&
    operator=(const optional<U> &rhs)
        noexcept(std::conjunction<
                    std::is_nothrow_assignable<T, const U &>,
                    std::is_nothrow_constructible<T, const U &>>::value)
        requires(std::conjunction<
                    detail::is_not_optional<U>,
                    std::is_assignable<T, const U &>,
                    std::is_constructible<T, const U &>>::value)
    {
        if (bool(rhs))
            {
                if (bool(*this))
                    {
                        **this = *rhs;
                    }
                else
                    {
                        std::construct_at(std::to_address(*this), *rhs);
                        this->m_engaged = true;
                    }
            }
        else
            {
                if (bool(*this))
                    {
                        this->reset ();
                    }
            }

        return *this;
    }

  // Converting move assignment ( Coercion by Member Template )
    template <typename U>
    constexpr optional& 
    operator=(optional<U> &&rhs)
        noexcept(std::conjunction<
                    std::is_nothrow_assignable<T, U &&>,
                    std::is_nothrow_constructible<T, U &&>>::value)
        requires(std::conjunction<
                    detail::is_not_optional<U>,
                    std::is_assignable<T, U &&>,
                    std::is_constructible<T, U &&>>::value)
    {
        if (bool(rhs))
            {
                if (bool(*this))
                    {
                        **this = std::move(*rhs);
                    }
                else
                    {
                        std::construct_at(std::to_address(*this), std::move(*rhs));
                        this->m_engaged = true;
                    }
            }
        else
            {
                if (bool(*this))
                    {
                        this->reset ();
                    }
            }

          return *this;
      }

  // Perfect-forwarded assignment
    template <typename U>
    constexpr optional&
    operator=(U &&value) noexcept(std::conjunction<
                                    std::is_nothrow_assignable<T, U &&>,
                                    std::is_nothrow_constructible<T, U &&>>::value)
        requires(std::conjunction<
                    std::is_assignable<T, U &&>,
                    std::is_constructible<T, U &&>>::value)
    {
        if (bool(*this))
            {
                **this = std::forward<U>(value);
            }
        else
            {
                std::construct_at(std::to_address(*this), std::forward<U>(value));
                this->m_engaged = true;
            }

        return *this;
    }

public : // Access contained value

    const_reference
    operator*() const noexcept
    {
        assert( bool(*this) && "Dereferencing disengaged optional" );
        return *( this->m_store );
    }

    reference
    operator*() noexcept
    {
        assert( bool(*this) && "Dereferencing disengaged optional");
        return *( this->m_store );
    }

    const_pointer
    operator->() const noexcept
    {
        assert( bool(*this) && "Accessing disengaged optional" );
        return std::addressof(**this);
    }

    pointer
    operator->() noexcept
    {
        assert( bool(*this) && "Accessing disengaged optional" );
        return std::addressof(**this);
    }

public: // checks whether the object contains a value
    constexpr
    operator bool() const noexcept { return this->m_engaged; }

    constexpr bool
    has_value() const noexcept { return this->m_engaged; }

public:
    // Observer the contained value
    const_reference
    value() const &
    {
        if (bool(*this))
            {
                return **this;
            }

        throw bad_optional_access{ "no value engaged!" };
    }

    // Observer the contained value
    reference
    value() &
    {
        if (bool(*this))
            {
                return **this;
            }

        throw bad_optional_access{ "no value engaged!" };
    }

    // returns the contained value if available, another value otherwise
    template <typename U> requires std::convertible_to<U, T>
    value_type
    value_or(U &&value) & noexcept(std::is_nothrow_convertible<U, T>::value)
    {
        if (bool(*this))
            {
                return **this;
            }

        return ((T)std::forward<U>(value));
    }

    // returns the contained value if available, another value otherwise
    template <typename U> requires std::convertible_to<U, T>
    value_type
    value_or(U &&value) && noexcept (std::is_nothrow_convertible<U, T>::value)
    {
        if (bool(*this))
            {
                return std::move(*(optional&)(*this));
            }
        return ((T)std::forward<U>(value));
    }

  public:
    // assignment ( constructs the contained value in-place )
    template <typename... ARGS> requires std::constructible_from<T, ARGS...>
    void
    emplace(ARGS &&...args)
        noexcept (std::is_nothrow_constructible<T, ARGS...>::value)
    {
        if (bool(*this))
            {
                this->reset ();
            }

        std::construct_at(std::to_address(*this), std::forward<ARGS>(args)...);
        this->m_engaged = true;
    }

  public:
    // Swap ( exchanges the contents )
    void swap(optional &rhs)
        noexcept(std::is_nothrow_swappable<T>::value)
        requires (std::is_swappable<T>::value)
    {
        if (bool(*this))
            {
                if (bool(rhs))
                    {
                        std::swap(**this, *rhs);
                    }
                else
                    {
                        std::construct_at(std::to_address(rhs), std::move(**this));
                        rhs.m_engaged = true;

                        this->reset ();
                    }
            }
        else
            {
                if (bool (rhs))
                    {
                        std::construct_at(std::to_address(*this), std::move(*rhs));
                        this->m_engaged = true;

                        rhs.reset ();
                    }
                else
                    {
                        return void ();
                    }
            }
    }

  public:
    // destroys any contained value
    void
    reset() noexcept (std::is_nothrow_destructible<T>::value)
    {
        if (bool(*this))
            {
                std::destroy_at(std::to_address(*this));
                this->m_engaged = false;
            }
    }

  public:
    // destroys the contained value, if there is one
    constexpr ~optional() noexcept (std::is_nothrow_destructible<T>::value)
        requires (std::is_trivially_destructible<T>::value)
    = default;

    // destroys the contained value, if there is one
    constexpr ~optional() noexcept(std::is_nothrow_destructible<T>::value)
        requires (std::conjunction<
                    std::negation<std::is_trivially_destructible<T>>,
                    std::is_destructible<T>>::value)
    {
        this->reset();
    }

private:
    bool      m_engaged;
    storage_t m_store;
};



template <typename T> union optional<T>::storage_t
{

 public: // type alias
    using value_type = T;
    using reference        = T&;
    using const_reference  = const T&;


public: // constructors
    constexpr storage_t () noexcept : m_empty{} {}

    template <typename U>
        requires std::constructible_from<T, U>
    constexpr storage_t (U &&value)
        noexcept(std::is_nothrow_constructible<T, U>::value)
    {
        std::construct_at(
            std::launder(reinterpret_cast<T*>(std::addressof(m_value))),
            std::forward<U>(value));
    }

    template <typename... ARGS>
        requires std::constructible_from<T, ARGS...>
    constexpr storage_t (in_place_t, ARGS &&...args)
        noexcept(std::is_nothrow_constructible<T, ARGS...>::value)
    {
        std::construct_at (
            std::launder(reinterpret_cast<T*> (std::addressof (m_value))),
            std::forward<ARGS> (args)...);
    }

    template <template <typename...> class Tuple, typename... ARGS>
        requires std::constructible_from<T, ARGS...>
    constexpr storage_t(from_tuple_t, const Tuple<ARGS...> &tuple)
        noexcept(std::is_nothrow_constructible<T, ARGS...>::value)
    {
        [&]<auto... Indx> (std::integer_sequence<std::size_t, Indx...>) {
            std::construct_at(std::addressof(**this), std::get<Indx>(tuple)...);
        }(std::make_integer_sequence<std::size_t, sizeof...(ARGS)>{});
    }

    template <typename U>
    constexpr storage_t&
    operator=(U &&value) noexcept (std::is_nothrow_assignable<T, U>::value)
        requires (std::is_assignable<T, U>::value)
    {
        **this = std::forward<U>(value);
        return *this;
    }

public: // Access the contained value

    const_reference
    operator*() const noexcept
    {
        return *std::launder(
            reinterpret_cast<const T*>(std::addressof(this->m_value)));
    }

    reference
    operator*() noexcept
    {
        return *std::launder(
            reinterpret_cast<T*>(std::addressof(this->m_value)));
    }

public: // ddestructors
    constexpr ~storage_t() noexcept(std::is_nothrow_destructible<T>::value)
        requires std::is_trivially_destructible<T>::value
    = default;

    constexpr ~storage_t () noexcept (std::is_nothrow_destructible<T>::value)
        requires (!std::is_trivially_destructible<T>::value)
    {}

private:
    struct { } m_empty{};
    alignas(T) unsigned char m_value[sizeof(T)];
};

template <typename T>
class optional<T&>
{
    static_assert(!std::same_as<std::decay_t<T>, nullopt_t>, "bad T");
    static_assert(!std::same_as<std::decay_t<T>, in_place_t>, "bad T");

private: // type alias
    using pointer = T*;
    using const_pointer   = const T*;
    using reference       = T&;
    using const_reference = const T&;

  public: // constructors
    constexpr optional() noexcept = default;

    constexpr optional(nullopt_t) noexcept : optional{} {}

    constexpr optional(T &value) noexcept
        : m_reference{std::addressof(value)}
    {}

    constexpr optional(in_place_t, T &value) noexcept (true)
        : m_reference{std::addressof(value)}
    {}
    
    constexpr optional(const optional &other) noexcept
        : m_reference{other.m_reference}
    {}

    constexpr optional(T &&) noexcept            = delete;
    constexpr optional(in_place_t, T&&) noexcept = delete;
    constexpr optional(optional&&) noexcept      = delete;


 public: // assignment
    constexpr optional&
    operator=(const optional &rhs) noexcept
    {
        this->m_reference = rhs.m_reference;
        return *this;
    }

    constexpr optional&
    operator=(optional&&) noexcept= delete;


public: // Access the contained value
    const_reference
    operator*() const noexcept
    {
        assert(bool(*this) && "Dereferencing disengaged optional");
        return *(this->m_reference);
    }

    reference
    operator*() noexcept
    {
        assert(bool(*this) && "Dereferencing disengaged optional");
        return *(this->m_reference);
    }

    const_pointer
    operator->() const noexcept
    {
        assert(bool(*this) && "Accessing disengaged optional");
        return this->m_reference;
    }
    
    pointer
    operator->() noexcept
    {
        assert(bool(*this) && "Accessing disengaged optional");
        return this->m_reference;
    }


public: // Observer the contained value
    const_reference
    value() const&
    {
        if (bool(*this))
            {
                return **this;
            }

        throw bad_optional_access{ "no value engaged!" };
    }

    reference
    value() &
    {
        if (bool(*this))
            {
                return **this;
            }

        throw bad_optional_access{ "no value engaged!" };
    }

    template <typename U> requires std::convertible_to<U, T>
    typename std::decay<T>::type
    value_or(U &value) noexcept(std::is_nothrow_convertible<U, T>::value)
    {
        if (bool(*this))
            {
                return **this;
            }

        return static_cast<std::decay_t<T>>(std::forward<U>(value));
    }


public: // assignment
    optional&
    emplace(T &value) noexcept
    {
        **this = std::addressof(value);

        return *this;
    }

    optional& emplace (T &&) noexcept = delete;

public:
    void
    swap (optional &rhs) noexcept
    {
        std::swap(*rhs, **this);
    }

  public: // checks whether the object contains a value
    constexpr
    operator bool () const noexcept
    {
        return ( this->m_reference != nullptr );
    }

    constexpr bool
    has_value() const noexcept
    {
        return (this->m_reference != nullptr);
    }


public:
    void
    reset() noexcept { this->m_reference = nullptr; }

public:
    constexpr ~optional () noexcept = default;

private:
    pointer m_reference = nullptr;
};


template <typename T> class optional<T &&>
{
    static_assert(sizeof(T) == 0UL, "disallowed for rvalue reference!");
};


/****** Relational operators ******/

template <typename T, typename U>
    requires std::equality_comparable<T> &&
             std::equality_comparable_with<U, T>
constexpr bool
operator== (optional<T> const &lhs, optional<U> const &rhs) noexcept
{
    return bool(rhs) && bool(lhs) ? (*lhs == *rhs) : false;
}

template <typename T, typename U>
    requires std::equality_comparable<T> &&
             std::equality_comparable_with<U, T>
constexpr bool
operator!= (optional<T> const &lhs, optional<U> const &rhs) noexcept
{
    return bool(rhs) && bool(lhs) ? (*lhs != *rhs) : false;
}

template <typename T, typename U>
    requires std::totally_ordered<T> &&
             std::totally_ordered_with<U, T>
constexpr bool
operator< (optional<T> const &lhs, optional<U> const &rhs) noexcept
{
    return bool(rhs) && bool(lhs) ? (*lhs < *rhs) : false;
}

template <typename T, typename U>
    requires std::totally_ordered<T> &&
             std::totally_ordered_with<U, T>
constexpr bool
operator<= (optional<T> const &lhs, optional<U> const &rhs) noexcept
{
    return bool(rhs) && bool(lhs) ? (*lhs <= *rhs) : false;
}

template <typename T, typename U>
    requires std::totally_ordered<T> &&
             std::totally_ordered_with<U, T>
constexpr bool
operator> (optional<T> const &lhs, optional<U> const &rhs) noexcept
{
    return bool(rhs) && bool(lhs) ? (*lhs > *rhs) : false;
}

template <typename T, typename U>
    requires std::totally_ordered<T> &&
             std::totally_ordered_with<U, T>
constexpr bool
operator>= (optional<T> const &lhs, optional<U> const &rhs) noexcept
{
    return bool(rhs) && bool(lhs) ? (*lhs >= *rhs) : false;
}

/***** Comparison with nullopt ******/

template <typename T>
constexpr bool
operator== (optional<T> const &lhs, nullopt_t) noexcept
{
    return !bool (lhs);
}

template <typename T>
constexpr bool
operator== (nullopt_t, optional<T> const &rhs) noexcept
{
    return !bool (rhs);
}

template <typename T>
constexpr bool
operator!= (optional<T> const &lhs, nullopt_t) noexcept
{
    return bool(lhs);
}

template <typename T>
constexpr bool
operator!= (nullopt_t, optional<T> const &rhs) noexcept
{
    return bool(rhs);
}

template <typename T>
constexpr bool
operator< (optional<T> const &, nullopt_t) noexcept
{
    return false;
}

template <typename T>
constexpr bool
operator< (nullopt_t, optional<T> const &rhs) noexcept
{
    return bool(rhs);
}

template <typename T>
constexpr bool
operator<= (optional<T> const &lhs, nullopt_t) noexcept
{
    return !bool (lhs);
}

template <typename T>
constexpr bool
operator<= (nullopt_t, optional<T> const &) noexcept
{
    return true;
}

template <typename T>
constexpr bool
operator> (optional<T> const &lhs, nullopt_t) noexcept
{
    return bool(lhs);
}

template <typename T>
constexpr bool
operator>(nullopt_t, optional<T> const &) noexcept
{
    return false;
}

template <typename T>
constexpr bool
operator>=(optional<T> const &, nullopt_t) noexcept
{
    return true;
}

template <typename T>
constexpr bool
operator>=(nullopt_t, optional<T> const &rhs) noexcept
{
    return !bool(rhs);
}

/** Comparison with  U **/

template <typename T, typename U>
    requires std::equality_comparable<T> &&
             std::equality_comparable_with<U, T>
constexpr bool
operator== (optional<T> const &lhs,
            typename std::type_identity<U>::type const &rhs) noexcept
{
    return bool(lhs) ? (*lhs == rhs) : false;
}

template <typename T, typename U>
constexpr bool
operator== (T const &lhs, optional<U> const &rhs) noexcept
{
    return bool(rhs) ? (lhs == *rhs) : false;
}

template <typename T, typename U>
    requires std::equality_comparable<T> &&
             std::equality_comparable_with<U, T>
constexpr bool
operator!= (optional<T> const &lhs,
            typename std::type_identity<U>::type const &rhs) noexcept
{
    return bool(lhs) ? (*lhs != rhs) : false;
}

template <typename T, typename U>
constexpr bool
operator!= (T const &lhs, optional<U> const &rhs) noexcept
{
    return bool(rhs) ? (lhs != *rhs) : false;
}

template <typename T, typename U>
    requires std::totally_ordered<T> &&
             std::totally_ordered_with<U, T>
constexpr bool
operator< (optional<T> const &lhs,
           typename std::type_identity<U>::type const &rhs) noexcept
{
    return bool(lhs) ? (*lhs < rhs) : false;
}

template <typename T, typename U>
constexpr bool
operator< (T const &lhs, optional<U> const &rhs) noexcept
{
    return bool(rhs) ? (lhs < *rhs) : false;
}

template <typename T, typename U>
    requires std::totally_ordered<T> &&
             std::totally_ordered_with<U, T>
constexpr bool
operator<= (optional<T> const &lhs,
            typename std::type_identity<U>::type const &rhs) noexcept
{
    return bool(lhs) ? (*lhs <= rhs) : false;
}

template <typename T, typename U>
constexpr bool
operator<= (T const &lhs, optional<U> const &rhs) noexcept
{
    return bool(rhs) ? (lhs <= *rhs) : false;
}

template <typename T, typename U>
    requires std::totally_ordered<T> &&
             std::totally_ordered_with<U, T>
constexpr bool
operator> (optional<T> const &lhs, typename std::type_identity<U>::type const &rhs) noexcept
{
    return bool(lhs) ? (*lhs > rhs) : false;
}

template <typename T, typename U>
constexpr bool
operator> (T const &lhs, optional<U> const &rhs) noexcept
{
    return bool(rhs) ? (lhs > *rhs) : false;
}

template <typename T, typename U>
    requires std::totally_ordered<T> &&
             std::totally_ordered_with<U, T>
constexpr bool
operator>= (optional<T> const &lhs,
            typename std::type_identity<U>::type const &rhs) noexcept
{
    return bool(lhs) ? (*lhs >= rhs) : false;
}

template <typename T, typename U>
constexpr bool
operator>= (T const &lhs, optional<U> const &rhs) noexcept
{
    return bool(rhs) ? (lhs >= *rhs) : false;
}

/** Specialized algorithms **/
template <typename T>
void
swap(optional<T> &lhs, optional<T> &rhs) noexcept (noexcept (lhs.swap (rhs)))
{
    lhs.swap (rhs);
}

template <typename T>
optional<typename std::decay<T>::type>
make_optional(T &&value)
{
    return optional<typename std::decay<T>::type> (std::forward<T> (value));
}

template <typename T, typename... ARGS>
optional<T>
make_optional(ARGS &&...args)
{
    return optional<T> (in_place, std::forward<ARGS> (args)...);
}

template <typename T, template <typename...> class Tuple, typename... ARGS>
optional<T>
make_optional(const Tuple<ARGS...> &tuple)
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
        constexpr auto
        operator()(argument_type const &arg) const -> result_type
        {
            return arg ? std::hash<T>{}(*arg) : result_type{};
        }
    };

    template <typename T>
    struct hash<::optional<T &>>
    {
        using result_type   = typename hash<T>::result_type;
        using argument_type = ::optional<T&>;

        constexpr auto
        operator()(argument_type const &arg) const -> result_type
        {
            return arg ? std::hash<T>{}(*arg) : result_type{};
        }
    };
}
#endif
