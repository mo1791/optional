#ifndef _OPTIONAL_HXX_
#define _OPTIONAL_HXX_


#include <concepts>
#include <iostream>


namespace std
{

    /** workaround for missing concepts in GCC and CLANG **/
    template <typename T>
    concept trivially_copy_constructible = is_trivially_copy_constructible<T>::value;

    template <typename T>
    concept trivially_move_constructible = is_trivially_move_constructible<T>::value;

    template <typename T>
    concept trivially_copy_assignable = is_trivially_copy_assignable<T>::value;

    template <typename T>
    concept trivially_move_assignable = is_trivially_move_assignable<T>::value;

    template <typename T>
    concept trivially_destructible = is_trivially_destructible<T>::value;
    /** END **/
}


/** class bad_optional_access **/
struct bad_optional_access : public std::logic_error
{
public:
    explicit bad_optional_access(const std::string& what_arg) : std::logic_error{what_arg} {}
    explicit bad_optional_access(const char* what_arg) : std::logic_error{what_arg} {}
};
/** END **/


inline constexpr struct trivial_init_t{} trivial_init{};

/** In-place construction **/
inline constexpr struct in_place_t {} in_place{};
/** END **/


/** Disengaged state indicator **/
struct nullopt_t
{
  struct init{};
  constexpr nullopt_t(init){};
};

inline constexpr nullopt_t nullopt{nullopt_t::init{}};
/** END **/



template <typename T>
union storage_t
{
    std::byte m_dummy;
    T m_value;

    constexpr storage_t(trivial_init_t) noexcept : m_dummy() {}
    
    template <typename ...ARGS>
    constexpr storage_t(ARGS&& ...args) noexcept requires( std::constructible_from<T, ARGS...>)
        : m_value(std::forward<ARGS>(args)...)
    {}

    constexpr ~storage_t() noexcept requires( std::trivially_destructible<T> ) = default;
    ~storage_t()           noexcept requires( not std::trivially_destructible<T> ) {}
};



/** optional for objects types **/
template <typename T>
class optional
{
    static_assert( not std::same_as<typename std::decay<T>::type, nullopt_t>, "bad T" );
    static_assert( not std::same_as<typename std::decay<T>::type, in_place_t>, "bad T" );

public: /** alias type **/
    using value_type       = T;
    using const_value_type = typename std::add_const<T>::type;
    using reference        = typename std::add_lvalue_reference<T>::type;
    using const_reference  = typename std::add_lvalue_reference<const_value_type>::type;
    using pointer          = typename std::add_pointer<T>::type;
    using const_pointer    = typename std::add_pointer<const_value_type>::type;

public: /** constructors **/

    constexpr optional() noexcept : m_engaged(false) , m_store(trivial_init) {}

    constexpr optional(nullopt_t) noexcept : optional() {}

    constexpr optional(T const& value) requires( std::copy_constructible<T> )
        : m_engaged(true)
        , m_store(value)
    {}

    constexpr optional(T&& value) noexcept requires( std::move_constructible<T> )
        : m_engaged(true)
        , m_store(std::move(value))
    {}

    template <typename ...ARGS>
    constexpr optional(in_place_t, ARGS&& ...args) noexcept
        : m_engaged(true)
        , m_store(std::forward<ARGS>(args)...)
    {}

    constexpr optional(optional const&) noexcept 
        requires( std::copy_constructible<T> && std::trivially_copy_constructible<T> ) = default;

    constexpr optional(optional const& rhs) noexcept 
        requires( std::copy_constructible<T> && not std::trivially_copy_constructible<T> )
    {
        if ( rhs.m_engaged )
        {
            ::new( std::addressof(m_store.m_value) ) T(*rhs); 
            
            m_engaged = true;
        }
    }

    constexpr optional(optional&&) noexcept
        requires( std::move_constructible<T> && std::trivially_move_constructible<T> ) = default;
    
    constexpr optional(optional&& rhs) noexcept
        requires( std::move_constructible<T> && not std::trivially_move_constructible<T> )
    {
        if ( rhs.m_engaged )
        {
            ::new( std::addressof(m_store.m_value) ) T(std::move(*rhs));

            m_engaged = true;
        }
    }
        
public: /** assignment **/
    constexpr auto operator=(optional const&) noexcept -> optional &
        requires( std::is_copy_assignable_v<T> && std::trivially_copy_assignable<T> ) = default;
    
    constexpr auto operator=(optional const& rhs) noexcept -> optional &
        requires( std::is_copy_assignable_v<T> && not std::trivially_copy_assignable<T> )
    {
        if ( rhs.m_engaged )
        {
            if ( m_engaged )
            {
                m_store.m_value = *rhs;
            }
            else {
                ::new( std::addressof(m_store.m_value) ) T(*rhs);

                m_engaged = true;
            }
        }
        else {
            if ( m_engaged )
            {
                m_store.m_value.T::~T();

                m_engaged = false;
            }
        }
        return *this;
    }

    constexpr auto operator=(optional&&) noexcept -> optional &
        requires( std::is_move_assignable_v<T> && std::trivially_move_assignable<T> ) = default;
    
    constexpr auto operator=(optional&& rhs) noexcept -> optional &
        requires( std::is_move_assignable_v<T> && not std::trivially_move_assignable<T> )
    {
        if ( rhs.m_engaged )
        {
            if ( m_engaged )
            {
                m_store.m_value = std::move(*rhs);
            }
            else {
                ::new( std::addressof(m_store.m_value) ) T(std::move(*rhs));

                m_engaged = true;
            }
        }
        else {
            if ( m_engaged )
            {
                m_store.m_value.T::~T();

                m_engaged = false;
            }
        }
        return *this;
    }

    template <typename U>
    constexpr auto operator=(U&& value) noexcept -> optional &
        requires( std::assignable_from<U, T> )
    {
        if ( m_engaged )
        {
            m_store.m_value = std::forward<U>(value);
        }
        else {
            ::new( std::addressof(m_store.m_value) ) T(std::forward<U>(value));
            m_engaged = true;
        }
        return *this;
    }

    constexpr auto operator=(nullopt_t) noexcept -> optional &
    {
        if ( m_engaged ) m_store.m_value.T::~T();

        m_engaged = false;
        
        return *this;
    }

public: /** Observers **/
    auto operator->() const noexcept -> const_pointer { return std::addressof(m_store.m_value); }
    auto operator->()       noexcept -> pointer       { return std::addressof(m_store.m_value); }

    auto operator*() const noexcept -> const_reference { return m_store.m_value; }
    auto operator*()       noexcept -> reference       { return m_store.m_value; }

public: /** Observers **/
    constexpr auto has_value() const noexcept -> bool { return m_engaged; }
    constexpr operator bool()  const noexcept         { return m_engaged; }

public: /** Observers **/
    auto value() const -> const_reference
    {
        return *this ? **this : throw bad_optional_access{"no value engaged!"};
    }
    auto value() 
    {
        return *this ? **this : throw bad_optional_access{"no value engaged!"};
    }

    template <typename U>
    auto value_or(U&& value) const& noexcept -> value_type
        requires( std::convertible_to<U, T> )
    {
        return *this ? **this : static_cast<T>(std::forward<U>(value));
    }

    template <typename U>
    auto value_or(U&& value) && noexcept -> value_type
        requires( std::convertible_to<U, T> )
    {
        return *this ? std::move( static_cast<optional&>(*this).m_store.m_value)
                     : static_cast<T>(std::forward<U>(value));
    }

public: /** assignment **/
    template <typename... ARGS>
    void emplace(ARGS&& ...args) noexcept requires( std::constructible_from<T, ARGS...>)
    {
        if ( m_engaged ) m_store.m_value.T::~T();

        ::new( std::addressof(m_store.m_value) ) T(std::forward<ARGS>(args)...);
        
        m_engaged = true;
    }

public: /** Swap **/
    void swap(optional& rhs) noexcept( noexcept( std::swap(std::declval<T&>(), std::declval<T&>()) ))
    {
        if ( m_engaged )
        {
            if ( rhs.m_engaged )
            {
                using std::swap;
                swap( **this, *rhs );
            }
            else {
                ::new( std::addressof(*rhs) ) T(std::move(**this));                
                rhs.m_engaged = true;

                m_store.m_value.T::~T();
                m_engaged = false;
            }
        }
        else {
            if ( rhs.m_engaged )
            {
                ::new( std::addressof(m_store.m_value) ) T(std::move(*rhs));
                m_engaged = true;

                rhs.m_store.m_value.T::~T();
                rhs.m_engaged = false;
            }
            else {
                return void();
            }
        }
    }

public: /** destructors **/
    constexpr ~optional() noexcept requires( std::trivially_destructible<T> ) = default;

    ~optional() noexcept requires( not std::trivially_destructible<T> )
    {
        if ( m_engaged ) m_store.m_value.T::~T();
    }


private:
    bool m_engaged { false };
    storage_t<T> m_store;
};


/** optional for lvalue reference types **/
template <typename T>
struct optional<T&>
{
    static_assert( not std::same_as<typename std::decay<T>::type, nullopt_t>, "bad T" );
    static_assert( not std::same_as<typename std::decay<T>::type, in_place_t>, "bad T" );

private: /** alias type **/
    using const_t             = typename std::add_const<T>::type;   
    using pointer             = typename std::add_pointer<T>::type;
    using const_pointer        = typename std::add_pointer<const_t>::type;
    using reference           = typename std::add_lvalue_reference<T>::type;
    using const_reference    = typename std::add_lvalue_reference<const_t>::type;

public: /** constructors **/
    constexpr optional()          noexcept : m_reference(nullptr) {}
    constexpr optional(nullopt_t) noexcept : m_reference(nullptr) {}
    constexpr optional(T& value)  noexcept : m_reference(std::addressof(value)) {}
    optional(T&&) noexcept = delete;

    constexpr optional(in_place_t, T& value) noexcept 
        : m_reference(std::addressof(value)) 
    {}

    optional(in_place_t, T&&) = delete;

    constexpr optional(optional const& rhs) noexcept : m_reference(rhs.m_reference) {}

public: /** assignment**/
    constexpr optional& operator=(nullopt_t) noexcept
    {
        m_reference = nullptr;

        return *this;
    }

    template <typename U>
    auto operator=(U&& rhs) noexcept -> optional& 
        requires( std::same_as<typename std::decay<U>::type, optional<T&>> )
    {
        m_reference = rhs.m_reference;

        return *this;
    }

    template <typename U>
    auto operator=(U&& rhs) noexcept -> optional& 
        requires( not std::same_as<typename std::decay<U>::type, optional<T&>> ) = delete;

public: /** Observers **/
    auto operator->() const noexcept -> const_pointer { return m_reference; }
    auto operator->()       noexcept -> pointer { return m_reference; }

    auto operator*() const noexcept -> const_reference { return *m_reference; }
    auto operator*()       noexcept -> reference { return *m_reference; }

public: /** Observers **/
    auto value() const -> const_reference
    {
        return *this ? **this : throw bad_optional_access{"no value engaged!"};
    }
    auto value() -> reference
    {
        return *this ? **this : throw bad_optional_access{"no value engaged!"};
    }

    template <typename U>
    auto value_or(U&& value) const noexcept -> typename std::decay<T>::type
        requires( std::convertible_to<U, typename std::decay<T>::type> )
    {
        return *this ? **this
                     : static_cast<typename std::decay<T>::type>(std::forward<U>(value));
    }

public: /** assignment **/
    auto emplace(T& value) noexcept -> optional &
    {
        m_reference = std::addressof(value);

        return *this;
    }

    auto emplace(T&&) noexcept -> optional & = delete;


public: /** Swap **/
    void swap(optional& rhs) noexcept( noexcept(std::swap(std::declval<T&>(), std::declval<T&>())) )
    {
        using std::swap;

        swap( rhs.m_reference, m_reference );
    }


public: /** Observers **/
    constexpr auto has_value() const noexcept -> bool { return m_reference != nullptr; }

    constexpr operator bool() const noexcept { return m_reference != nullptr; }

public: /** destructor **/
    constexpr ~optional() noexcept = default;

private:
    pointer m_reference;
};


template <typename T>
struct optional<T&&>
{
    static_assert( std::bool_constant<false>::value, "optional rvalue referencs disallowed" );
};



/****** Relational operators ******/

template <typename T, typename U>
requires requires(const std::remove_reference_t<T>& lhs,
                  const std::remove_reference_t<U>& rhs )
    { { lhs == rhs } -> std::convertible_to<bool>; }
constexpr bool operator==(optional<T> const& lhs, optional<U> const& rhs) noexcept
{
    return bool(rhs) && bool(lhs) ? ( *lhs == *rhs ) : false;
}

template <typename T, typename U>
requires requires(const std::remove_reference_t<T>& lhs,
                  const std::remove_reference_t<U>& rhs )
    { { lhs != rhs } -> std::convertible_to<bool>; }
constexpr bool operator!=(optional<T> const& lhs, optional<U> const& rhs) noexcept
{
    return bool(rhs) && bool(lhs) ? ( *lhs != *rhs ) : false;
}

template <typename T, typename U>
requires requires(const std::remove_reference_t<T>& lhs,
                  const std::remove_reference_t<U>& rhs )
    { { lhs < rhs } -> std::convertible_to<bool>; }
constexpr bool operator<(optional<T> const& lhs, optional<U> const& rhs) noexcept
{
    return bool(rhs) && bool(lhs) ? ( *lhs < *rhs ) : false;
}

template <typename T, typename U>
requires requires(const std::remove_reference_t<T>& lhs,
                  const std::remove_reference_t<U>& rhs )
    { { lhs <= rhs } -> std::convertible_to<bool>; }
constexpr bool operator<=(optional<T> const& lhs, optional<U> const& rhs) noexcept
{
    return bool(rhs) && bool(lhs) ? ( *lhs <= *rhs ) : false;
}


template <typename T, typename U>
requires requires(const std::remove_reference_t<T>& lhs,
                  const std::remove_reference_t<U>& rhs )
    { { lhs > rhs } -> std::convertible_to<bool>; }
constexpr bool operator>(optional<T> const& lhs, optional<U> const& rhs) noexcept
{
    return bool(rhs) && bool(lhs) ? ( *lhs > *rhs ) : false;
}

template <typename T, typename U>
requires requires(const std::remove_reference_t<T>& lhs,
                  const std::remove_reference_t<U>& rhs )
    { { lhs >= rhs } -> std::convertible_to<bool>; }
constexpr bool operator>=(optional<T> const& lhs, optional<U> const& rhs) noexcept
{
    return bool(rhs) && bool(lhs) ? ( *lhs >= *rhs ) : false;
}



/***** Comparison with nullopt ******/

template <typename T>
constexpr bool operator==(optional<T> const& lhs, nullopt_t) noexcept
{
    return not( bool(lhs) );
}

template <typename T>
constexpr bool operator==(nullopt_t, optional<T> const& rhs) noexcept
{
    return ::operator==(rhs, nullopt);
}

template <typename T>
constexpr bool operator!=(optional<T> const& lhs, nullopt_t) noexcept
{
    return bool(lhs);
}

template <typename T>
constexpr bool operator!=(nullopt_t, optional<T> const& rhs) noexcept
{
    return ::operator!=(rhs, nullopt);
}

template <typename T>
constexpr bool operator<(optional<T> const&, nullopt_t) noexcept
{
    return false;
}

template <typename T>
constexpr bool operator<(nullopt_t, optional<T> const& rhs) noexcept
{
    return bool(rhs);
}

template <typename T>
constexpr bool operator<=(optional<T> const& lhs, nullopt_t) noexcept
{
    return not( bool(lhs) );
}

template <typename T>
constexpr bool operator<=(nullopt_t, optional<T> const&) noexcept
{
    return true;
}

template <typename T>
constexpr bool operator>(optional<T> const& lhs, nullopt_t) noexcept
{
    return bool(lhs);
}

template <typename T>
constexpr bool operator>(nullopt_t, optional<T> const&) noexcept
{
    return false;
}

template <typename T>
constexpr bool operator>=(optional<T> const&, nullopt_t) noexcept
{
    return true;
}

template <typename T>
constexpr bool operator>=(nullopt_t, optional<T> const& rhs) noexcept
{
    return not( bool(rhs) );
}



/** Comparison with  U **/

template <typename T, typename U >
requires requires(const std::remove_reference_t<T>& lhs,
                  const std::remove_reference_t<U>& rhs )
    { { lhs == rhs } -> std::convertible_to<bool>; }
constexpr bool operator==(optional<T> const& lhs, U const& rhs) noexcept
{
    return bool(lhs) ? ( *lhs == rhs ) : false;
}

template <typename T, typename U>
constexpr bool operator==(T const& lhs, optional<U> const& rhs) noexcept
{
    return ::operator==(rhs, lhs);
}

template <typename T, typename U>
requires requires(const std::remove_reference_t<T>& lhs,
                  const std::remove_reference_t<U>& rhs )
    { { lhs != rhs } -> std::convertible_to<bool>; }
constexpr bool operator!=(optional<T> const& lhs, U const& rhs) noexcept
{
    return bool(lhs) ? ( *lhs != rhs ) : false;
}

template <typename T, typename U>
constexpr bool operator!=(T const& lhs, optional<U> const& rhs) noexcept
{
    return ::operator!=(rhs, lhs);
}

template <typename T, typename U>
requires requires(const std::remove_reference_t<T>& lhs,
                  const std::remove_reference_t<U>& rhs )
    { { lhs < rhs } -> std::convertible_to<bool>; }
constexpr bool operator<(optional<T> const& lhs, U const& rhs) noexcept
{
    return bool(lhs) ? ( *lhs < rhs ) : false;
}

template <typename T, typename U>
constexpr bool operator<(T const& lhs, optional<U> const& rhs) noexcept
{
    return ::operator<(rhs, lhs);
}

template <typename T, typename U>
requires requires(const std::remove_reference_t<T>& lhs,
                  const std::remove_reference_t<U>& rhs )
    { { lhs <= rhs } -> std::convertible_to<bool>; }
constexpr bool operator<=(optional<T> const& lhs, U const& rhs) noexcept
{
    return bool(lhs) ? ( *lhs <= rhs ) : false;
}

template <typename T, typename U>
constexpr bool operator<=(T const& lhs, optional<U> const& rhs) noexcept
{
    return ::operator<=(rhs, lhs);
}


template <typename T, typename U>
requires requires(const std::remove_reference_t<T>& lhs,
                  const std::remove_reference_t<U>& rhs )
    { { lhs > rhs } -> std::convertible_to<bool>; }
constexpr bool operator>(optional<T> const& lhs, U const& rhs) noexcept
{
    return bool(lhs) ? ( *lhs > rhs ) : false;
}

template <typename T, typename U>
constexpr bool operator>(T const& lhs, optional<U> const& rhs) noexcept
{
    return ::operator>(rhs, lhs);
}

template <typename T, typename U>
requires requires(const std::remove_reference_t<T>& lhs,
                  const std::remove_reference_t<U>& rhs )
    { { lhs >= rhs } -> std::convertible_to<bool>; }
constexpr bool operator>=(optional<T> const& lhs, U const& rhs) noexcept
{
    return bool(lhs) ? ( *lhs >= rhs ) : false;
}

template <typename T, typename U>
constexpr bool operator>=(T const& lhs, optional<U> const& rhs) noexcept
{
    return ::operator>=(rhs, lhs);
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
        using argument_type = optional<T>;
    
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
        using argument_type = optional<T&>;
        
        constexpr auto operator()(argument_type const& arg) const -> result_type
        {
            return arg ? std::hash<T>{}(*arg) : result_type{};
        }
    };
}

#endif