# README

## Overview

Due to the way the Erlang/OTP `gen_event` behaviour works, both the event
manager and the handlers are executed within the same process. In the
context of `lager`, this reasoning applies to the `lager_event` process and its
backends.

Sometimes it is a good idea to decouple the event handlers from the
event manager and to run them in separate processes. The
`lager_middleman_backend` backend does exactly this, by inserting a
middleman process between `lager_event` and your backends. It has a
1:1 mapping with the backends, so you can specify exactly which
backends want to use a middleman.

## Usage

To use the `lager_middleman_backend` backend, simply include this
repository as a dependency for your project (e.g. by
adding an entry to your `rebar.config`) and wrap one or more lager
backends into a middleman tuple, as explained below.

If the name of the lager backend that you want to wrap is an atom:

```
    {Name :: atom(), Config}
```

Simply convert it to:

```
    {{lager_middleman_backend, Name}, {Backend, Config}}
```

If your lager backend contains an `id`:

```
    {{Name :: atom(), Id :: term()}, Config}
```

Convert it to:

```
    {{lager_middleman_backend, {Name, Id}}, {Name, Config}}
```

For example:

```
    {lager_console_backend, info}
```

Would become:

```
    {lager_middleman_backend, lager_console_backend}, {lager_console_backend, debug}}
```

And:

```
    {{lager_file_backend, "debug.log"}, {"debug.log", debug, 10485760, "$D0", 5}}.
```

Would become:

```
    {{lager_middleman_backend, {lager_file_backend, "debug.log"}}, {lager_file_backend, {"debug.log", debug, 10485760, "$D0", 5}}}.
```

That's it.

## When is the middleman useful?

We found this middleman particularly useful in the case of the
`lager_file_backend`, shipped by default with lager, since the backend suffers a
performance issue in the case the `lager_event` mailbox gets full.

In fact, due to the way the Erlang I/O system works, the time for
writing to file is directly proportional to the length of the mailbox
of the writing process. This means that, for the `lager_file_backend`,
file writing time is directly proportional to the message queue of the
`lager_event` process, which in some cases (especially when the
`async_threshold` is not used) can be huge.

## Notes

Please note that, in case of the `lager_file_backend`, it will not be
possible to use the nice _handlers expansion_ which allows you to pass
a list of _files_ to the same `lager_file_backend`, since the
expansion is currently hard-coded in `lager_app.erl`. Instead, you
will have to specify one single entry for each of your files.
