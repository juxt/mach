# Mach

In the beginning, we built code with custom shell scripts.

The problem with shell scripts was they tended to be dumb, knowing
only how to build the entire project from scratch.

Then along came Make, which was a particularly great tool for building code.

## Make: The Good, the Bad and the Ugly

The good parts of make were very good:

- incremental builds
- suffix rules

By deductions based off the
modification times of source and object files, make would quickly
identify where work was needed to be done, and go ahead and do
it. Unlike shell scripts, make was optimised around the goal of
building code incrementally.

Make was built around the concept of dependencies:

```
a.o:   b.c c.c
       cc -o a.o b.c c.c
```

Here, b.c and c.c might be source files. If either of them were newer
than a.o, or if a.o didn't exist, then the command `cc -o a.o b.c c.c`
would be run.

You could rewrite this make target using 'automatic variables' like this:

```
a.o:   b.c c.c
       cc -o $@ $?
```

Make goes downhill rapidly from here, forcing you to learn a ton of
stuff that nowadays looks and feels antiquated.

But Make was very flexible, and could be used to build almost
anything that could be expressed in dependency and target files.

Unfortunately, make syntax was baroque. Make relied on unintuitive
'features' whenever it needed to compute something, manipulate strings
or lists, and even required tabs in the right places, and might
silently fail if you forgot the rules.

Make was also not designed for the hierarchical file system used by
Java and others, and for this reason other tools such as Ant, Ivy,
Maven and Gradle came along and supplanted Make. However, these tools
tended to go back to the idea that it was good to build code from
scratch, and people got used to the idea that it was OK to wait ages
for something to build. Instead of fixing the problem, they swept the
problem away under an out-of-sight build machine or 'continuous
integration' server, turning adversity into virtue.

Then Clojure came along and people built things like Leiningen and
boot. These suffer from painfully slow start-up times. Leiningen is
really a Clojure launch tool rather than a build tool. Boot's name at
least indicates what it's best for: booting a powerful development
environment.

## Weakening abstraction

Maven and Gradle take an opinionated view of how projects, Java
project in particular, should be built, tested and released.

I don't think it is the job of a build tool to encourage, let alone
prescribe, a particular methodology in how projects should be
built. That is the job for books and blog articles.

## Introducing Mach

Mach is a remake of make, striving to keep the good parts.

### Sprechen Sie Deutsch?

Since you ask, the name is from the German verb, _machen_ (to do, to
make), used in the imperative. Mach is as much about 'doing' as
'making', which the German verb captures well.

It is also named after Austrian physicist, Ernst Mach, who's name is
given to the Mach number, which evokes speed.

### Design goals

- Fast start-up (ideally sub-second)
- Incremental builds (only do necessary work)
- Sane language (avoid make's horrible syntax and language features)
- Support rules and extensibility

For the language, we have chosen ClojureScript because it is clean,
consistent, expressive and powerful.

### Status

Sorry to say, this library is in experimental alpha and you shouldn't
use it, unless you really want to.

## Usage

Install [lumo](https://github.com/anmonteiro/lumo). Run with

Add a Makefile.edn file to your project.

```
$ lumo /path/to/mach/core.cljs
```

### Makefile syntax

Your first Makefile.edn might be a very simple affair:

```clojure
{
 hello (printn "Hello World!")
}
```

## Differences with make

### EDN format

Makefiles are data, and we have chosen the EDN format. If the only
reason EDN was better than JSON was because it allows comments, that
would be reason enough to choose it. It has many other advantages
though.

The Makefile is a map, modelled in a similar fashion to the original
Makefile, with targets as keys and dependencies/actions as values.

### Targets

Make targets were meant to be files. However, you'd often want a task
such as 'clean'. To ensure Make wouldn't confuse 'clean' with a target
file, you would have to declare clean in a .PROXY declaration, one of
many of Makes esoteric conventions.

With mach, consistency is favoured over terseness. All keys are names,
not target files. Target files are specified in the values.

### Value maps

Makefile entries have names (the keys) and values (maps which describe
the entry). Each value is itself a map. You can put anything you like
in this map, but keys with the `mach` namespace are reserved.

A few of these keys are special:

#### mach/depends

#### mach/novelty

#### mach/update!

### Refs

Make makes heavy use of variables, in the spirit of DRY (Don't Repeat
Yourself).

Often, this leads to obfuscation, variables are defined in terms of
other variables, and so on.

mach achieves DRY without endless indirection by using references (the
same way [Aero](https://github.com/juxt/aero) does it) - the value of
something can be left inline and that value can be referenced from
other parts of the Makefile.

## Built on nodejs

Mach sits on the extensive nodejs eco-system.

## Influences

Mach is influenced by the following:

Make, particularly GNU Make
Jake


## Road map

The goal of Mach is to create something that is capable of building
complex systems as well as running them.