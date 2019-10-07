# Getting Started

## Types

D-Bus has it's own type system. This library represents with the `DBusType` type.
The possible types are:

* Simple types:
    * Booleans
    * Signed and unsigned integers of length 8, 16, 32 and 64
    * Doubles
    * Unix File Descriptors
    * Strings
    * Object Paths
    * Type Signatures
* Composite Types:
    * Array (variable-length collection of values of the same type)
    * Structs (aka tuples)
    * Dicts (aka Map) where the key has to be a simple type
    * Variant (dynamically typed values)

See also the
[D-Bus specification](https://dbus.freedesktop.org/doc/dbus-specification.html#type-system)
for more information.

DBus values are represented by the GADT `DBusValue`. Every value is annotated with its `DBusType`. This ensures that we
can always create a type signature, even for empty arrays. It also ensures that
all elements of an array are of the same type and we hence know that every
constructable value has a sound type in D-Bus' type system.

For example, we have

```haskell
DBVByte 12 :: DBusValue ('DBusSimpleType 'TypeByte)
```

a D-bus Byte of value 12.

Note that `DBusSimpleType TypeByte` is a Haskell _value_ promoted to the type level.

### Representable Types

Because we would usually prefer to work with Haskell's type system rather than
the (somewhat cumbersome) D-Bus types, d-bus provides the `Representable` type
class for easy translation between Haskell and D-Bus types.

To write an instance of `Representable`, we have to provide three pieces of information:
* The D-Bus type a Haskell type should map to (Many Haskell types can map to a
  DBusType, but every Haskell type can only map to one DBus type).
* A function to convert a Haskell-value to a D-Bus value
* A function to parse a D-Bus value back to a Haskell value (may fail)

For example, let's consider the type

```haskell
data Foo = Foo Bool Text
```

We would like to translate this type to a D-Bus struct. Hence:

```haskell

instance Representable Foo where
  type RepType Foo = 'TypeStruct '[ 'DBusSimpleType 'TypeBoolean
                                  , 'DBusSimpleType 'TypeString
                                  ]
  toRep (Foo b t) = DBVStruct (StructCons (toRep b) (StructSingleton $ toRep t))
  fromRep (DBVStruct
           (StructCons b
            (StructSingleton t))) = do
    -- This is in the Maybe monad
    b' <- fromRep b
    t' <- fromRep t
    return $ Foo b' t'
```

Note how we only have to match one length of struct. The dbus-type ensures that
the length must match (and ideed, GHC would not allow us to try and match any
other length, as that would be a type error)

### Automatically generating Representable instances

Most Representable instances are boring. For example, there's only one "obvious"
way to translate our `Foo` type to D-Bus. So it would be convenient if we could
automate the creation of `Representable` instances, and indeed we can, with a little Template Haskell:

```haskell
data Foo = Foo Bool Text

makeRepresentable ''Foo
```

will create the same instance as we have written above. For a more in-depth
discussion of the rules of how those instances are generated, consult the
documentation of `makeRepresentable`

## Creating a connection

Before we can talk to another D-Bus entity, we need to connect to a message bus
and create `DBusConnection`. To do this, we call the `connectClient` function.
It takes an argument of type `ConnectionType` that determines which message bus we
will connect to and can be one of `System`, `Session` and `Address`, where
`System` and `Session` connect to the system and session bus
respectively. `Address` takes a string as a parameter and connects to the bus
found at this address

```haskell
con <- connectClient Session
```

## Invoking a method

Now that we have connected to the bus, we can invoke methods on other entities.

In order to call a method, we need to know some information about it:
* The entity, that is the name of the client connected to the bus we want to talk to
* The object we want to call
* The interface name the method belongs to
* the method name

To find this data, you can use introspection (see later in this
tutorial). d-feet is a GUI program that allows you to easily inspect the
exported methods of connected clients

For example, let's try and retrieve the number of unread feed items from liferea.
To do that, we use "callMethod" using 7 arguments

* The connection name, for liferea, that's "net.sourceforge.liferea"
* The object (path): "/org/gnome/feed/Reader"
* The interface: "org.gnome.feed.Reader"
* The method: "GetUnreadItems"
* Any arguments we want to pass to the method. "getUnreadItems" doesn't expect
  any, so we pass in ()
* Flags we want to pass. We don't need any, so we pass the empty list
* A connection object to send the request over

It will return `Either` a `MethodError` in case something went wrong or the
result with a polymorphic type. We will have to instantiate the result type by
giving a type anotation, because otherwise the marshaller doesn't know how to
handle the result.

Here's a complete example:

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Main where

import DBus
import Data.Int

main = do
  con <- connectClient Session
  result <- callMethod "net.sourceforge.liferea"
                       "/org/gnome/feed/Reader"
                       "org.gnome.feed.Reader"
                       "GetUnreadItems"
                       ()
                       []
                       con
                       :: IO (Either MethodError Int32)
  case result of
    Left e -> error $ "something went wrong " ++ show e
    Right unread -> putStrLn $ "We have " ++ show unread ++ " unread items"
```

## Method Descriptions

Instead of passing all the data by hand every time as we have done before, it
can be useful to collect them once and then reuse them.  This is also great when
 writing a library to export the method definitions

Here's an example:

```haskell
unreadItems :: MethodDescription '[] '['DBusSimpleType 'TypeInt32]
unreadItems =
  MD { methodObjectPath = "/org/gnome/feed/Reader"
     , methodInterface = "org.gnome.feed.Reader"
     , methodMember = "GetUnreadItems"
     , methodArgs = Done
     , methodResult = "unread items" :> Done
     }
```

We __have__ to give the type anotation to tell dbus what the argument and result
types are. Here we tell it to expect no arguments , so we have an empty
list. (Note that this list is _promoted_ to type level) and that we expect a
single argument of `DBusSimpleType TypeInt32` (a single 32-bit signed integer)
in return.

The `methodObjectPath`, `methodInterface` and `methodMember` fields
are pretty much what you expect.

`methodArgs` and `methodResult` are special "lists" of `Text`s (type
`ArgumentDescription` that describe the arguments. To construct one of those,
you can use Done for the empty list (similar to the `[]` constructor) and `:>`
to cons an element (similar to the `:` constructor). The values are purely
descriptive and you can leave them empty ("") if you want, but the _number_ of
them has to match the arity of the method description, that is, if the method
has 2 arguments, you have to give 2 descriptions in `methodArgs`, and similarly
for `methodResult`.

### Generating method descriptions from Introspection data

Writing method descriptions by hand is pretty tedious. Instead we can use
template haskell to auto-generate them for us from introspection xml.

To do that we need to first get the introspection xml. This d-bus library comes
with a dbus-introspect executable that can retrieve it from a running client. Or
we could use d-feet (find the "Introspect" method in the
"org.freedesktop.DBus.Introspectable" interface and call it without arguments).
Lastly, the documentation of the program you want to interface to might provide it.

Saving the xml in a file at out project root (in this example we will call it
"liferea.xml") we can now convert it into automatically generated definitions
using template haskell. It's a good idea to do this in another module:

```haskell
{-# LANGUAGE TemplateHaskell #-}

module Remote where

import DBus

-- Load introspection data and create definitions
makeDbusEndpoints def "liferea.xml"
```

this will create the following definitions for us:

```haskell
ping :: MethodDescription '[] '['DBusSimpleType 'TypeBoolean]
setOnline :: MethodDescription '['DBusSimpleType 'TypeBoolean] '['DBusSimpleType 'TypeBoolean]
subscribe :: MethodDescription '['DBusSimpleType 'TypeString] '['DBusSimpleType 'TypeBoolean]
getUnreadItems :: MethodDescription '[] '['DBusSimpleType 'TypeInt32]
getNewItems :: MethodDescription '[] '['DBusSimpleType 'TypeInt32]
refresh :: MethodDescription '[] '['DBusSimpleType 'TypeBoolean]
```

### Calling method descriptions

Once we have obtained our methodDescription, calling it is as easy as

```haskell
call getUnreadItems "net.sourceforge.liferea" () [] con
```

That is, apart from the method description we only have to pass in
* The entity name we want to call
* The arguments (Still none, in our case)
* Flags
* And the connection object

Note that the type of the method description fixes the argument and return
types. So we can't accidently pass in the wrong argument type or try to get out
the wrong types. However, d-bus will freely convert between haskell and d-bus
types for us according to `Representable` instances. So we can use either D-Bus
types or haskell types.

## Listening for signals

* TODO
