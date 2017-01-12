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

## Introducing Mach

Mach is a remake of make, striving to keep the good parts.

### Sprichst Du Deutsch?

Since you ask, the name is from the German verb, _machen_ (to do, to
make), used in the imperative. Mach is as much about 'doing' as
'making', which the German verb captures well.

### Design goals

- Fast start-up (ideally sub-second)
- Incremental builds (only do necessary work)
- Sane language (avoid make's horrible syntax and language features)
- Support rules and extensibility

For the language, we have chosen ClojureScript because it is clean,
consistent, expressive and powerful.

### Status

Mach is in alpha status. You are encouraged to use it if you are
prepared to accept some changes if you upgrade. The design of Mach is
likely to change in the near future.

## Install Mach

Install Mach with NPM like this

```
$ sudo npm install -g @juxt/mach
```

## Getting started

Create a simple project and add a Machfile.edn file.

Your very first Machfile.edn might look like this:

```clojure
{
 hallo (println "Guten Tag, Welt!")
}
```

You can invoke Mach with the following:

```
$ mach hallo
```

to get the following output:

```
Guten Tag, Welt!
```

### Try it out

Clone the mach project.

```
$ git clone https://github.com/juxt/mach
$ cd mach/test
```

Try the following:

```
$ mach target
```

This creates a directory called `target`.

Try again.

```
$ mach target
```

Since `target` already exists, you should get the following output:

```
Nothing to do!
```

Now try this:

```
$ mach css
```

If you have the SASS compiler installed, `sassc`, this will compile
the sass files in `sass` to `target/app.css`.

### Machfile syntax

Examine the file `Machfile.edn` in `test/`.

This contains a map between targets and target recipes.

A recipe can be either an expression or map.

## Differences with make

### EDN format

Machfiles are data, and we have chosen the EDN format. If the only
reason EDN was better than JSON was because it allows comments, that
would be reason enough to choose it. It has many other advantages
though.

The Machfile is a map, modelled in a similar fashion to the original
Machfile, with targets as keys and dependencies/actions as values.

### Targets

In Make, targets were intended to be files. However, you'd often want a task
such as 'clean'. To ensure Make wouldn't confuse 'clean' with a target
file, you would have to declare clean in a `.PROXY` declaration, one of
many of Makes esoteric conventions.

With mach, consistency is favoured over terseness. All keys are names,
not target files. Target files are specified in the values, where necessary.

### Novelty detection

Using symbols for targets, rather than files, solves another problem
with Make: support for directories.

In Unix, a directory's modification time reflects the last time a
directory was modified (files were added or removed), rather than the
last time _anything_ in the directory (or sub-directories) changed
(which is what you usually want for determining novelty).

Mach allows you to write functions (in ClojureScript!) for determining
novelty (i.e. whether a target is fresh or stale, and how so).

## Recipes

Machfile entries have target names (the keys) and recipes (actions or
maps which describe when and how to build the target). If you use a
map, you can put anything you like in this map, but keys with the
`mach` namespace are reserved.

A few of these keys are special:

### description

A short string of descriptive text

```clojure
{css {description "CSS for the website, compiled from sass sources"}}
```

### depends

The `depends` entry contains a list of targets that must be
updated (if stale) prior to this target being considered for update.

```clojure
{jar {description "A jar file containing Java classes and CSS"
      depends [classes css]}}
```

A `depends` is simply a sequence of targets to check.

### novelty

Deciding whether a target is stale (requiring a re-build) or fresh (no
re-build is necessary) might be a simple procedure or a complex
computation. Regardless, Mach asks you to provide a predicate, written
in ClojureScript to determine whether or not a target is stale.

Mach provides a built-in predicate to determine if any files in a
given directory have been modified with respect to a given file.

The signature of this function is:

```clojure
(mach.core/modified-since [file dir])
```

The first argument is the file with which all files in the second
argument (directory) are compared against. If any file in the
directory has been modified more recently than the file in the first
argument, the predicate returns true.

For example:

```clojure
{css {novelty (mach.core/modified-since "target/app.css" "sass")}}
```

It is also possible to express this recipe like this:

```clojure
{css {target "target/app.css"
      novelty (mach.core/modified-since target "sass")}}
```

This illustrates that symbols in ClojureScript expressions are
resolved with respect to the given map, which defines the scope for
all expressions. This allows you to surface key values as
declarations, accessible from within local ClojureScript expressions
and also exposed to other tools. These values are also accessible by
other targets, via references (see below).

### update!

If novelty is detected, a target is updated by calling the `update!`
function. The terminology here is intended to align with our
[skip](https://github.com/juxt/skip) project.

The `update!` expression must do whatever is necessary to rebuild
(freshen) the target.

```clojure
{css {target "target/app.css"
      novelty (mach.core/modified-since target #ref [sass dir])
      update! (apply mach.core/sh (concat ["sassc"] novelty [">" target]))}}
```

In the `update!` expression can be side-effecting (and should
be!). Often, an `update!` expression will reference the value of
`novelty` to reduce work.

## Calling out to the shell

One of the best design decisions in the original Make tool was to
integrate closely with the Unix shell. There are countless operations
that are accessible via the shell, and Mach strives to encourage this
usage via its custom EDN tag literal `#$`.

```clojure
{hello-internal (println "Hello World!")
 hello-external #$ ["echo Hello!"]}
 ```

The `#$` tag literal is a short-cut to the built-in Mach function
`mach.core/sh`.

## References

Make makes heavy use of _variables_, in the spirit of DRY (Don't
Repeat Yourself). Often, this leads to obfuscation, variables are
defined in terms of other variables, and so on.

Mach achieves DRY without endless indirection by using references (the
same way [Aero](https://github.com/juxt/aero) does it) - key values
can be declared in a recipe and referenced from other parts of the
Machfile, via the `#ref` tag literal.

```clojure
{
src {dir "src"}
classes {update! (compile #ref [src dir])}
}
```

The `#ref` tag must be followed by a vector of symbols which target
the required value.

## Built on nodejs

Mach sits on the extensive nodejs eco-system. If you need to do
anything remotely complex, feel free to pick from the
quarter-million-plus modules available to you.

## Thanks

Mach is built on [lumo](https://github.com/anmonteiro/lumo) by António
Nuno Monteiro.

## Influences

Mach is influenced by Make, particularly GNU Make, which has survived
the test of time (but not without baggage).

I also looked at Jake, which is a worthy re-implementation of Make,
sticking close to the original.

## Road map

The goal of Mach is to create something that is capable of building
complex systems as well as running them.
