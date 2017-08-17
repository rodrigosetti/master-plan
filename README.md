# master-plan

[![Build Status](https://travis-ci.org/rodrigosetti/master-plan.svg?branch=master)](https://travis-ci.org/rodrigosetti/master-plan)

Master Plan is a text based project management tool that implements an
algebra of projects.

These are the values propositions of master plan:

 * **Simplicity**: keep project management into a single text file. Under version control,
   close to your code.
 * **Agility**: embrace change, by allowing projects to specify uncertainty and allow
   for refinement anytime.
 * **Freedom**: master plan is a open specification, not dependent on tools or hosting.
   There is this current open-source implementation, but anyone can implement
   tools or visualizations on top of it.

See the [wiki](https://github.com/rodrigosetti/master-plan/wiki) for details and examples.

## Algebra of Projects

In the algebra of projects, a project is an expression of sub-projects
combined using dependency operators. These operators define how sub-projects
relate to the higher-level projects in terms of execution and structural
dependency, that is, in which order (if any) the sub-projects must be executed,
and also whether all or some of the sub-projects must be executed at all.

At some level, sub-projects will be small enough that they don't break down
further, in this case, they consist of a unit of execution.

There is also the notion cost estimation and risk. Cost may mean different
things depending on the domain, but most usually it's time.

Given all these constraints and structure, master plan will build an optimum
prioritization of projects and sub-projects for execution.

The entire definition of a project is defined into a single `.plan` file using a
simple language. There are defaults for most constrains and properties such that
things can be less verbose if using the defaults.

The tool is able to build visualizations from the plan file.

Ideally, the plan file should be kept in version control so that execution and
planning progress can be recorded.

### Command line Arguments

```
master-plan - project management tool for hackers

Usage: master-plan [FILENAME] [-o|--output FILENAME] [--progress-below N]
                   [--render-parse-error] [-c|--color] [-w|--width NUMBER]
                   [--height NUMBER] [-r|--root NAME]
                   [--hide title|description|url|owner|cost|trust|progress]
  See documentation on how to write project plan files

Available options:
  FILENAME                 plan file to read from (default from stdin)
  -o,--output FILENAME     output file name (.png, .tif, .bmp, .jpg and .pdf
                           supported)
  --progress-below N       only display projects which progress is < N%
  --render-parse-error     instead of printing parsing errors, render as an
                           image
  -c,--color               color each project by progress
  -w,--width NUMBER        width of the output image
  --height NUMBER          height of the output image
  -r,--root NAME           name of the root project definition (default: "root")
  --hide title|description|url|owner|cost|trust|progress
                           hide a particular property
  -h,--help                Show this help text
```

### Syntax

Comments are C/C++/Java style: line comments start with `//`, and block comments
are in between `/*` and `*/`.

Everything else are definitions, in the form `name [attributes] [expression] ;`.

A project name should be unique. Definitions end with semicolon.

Project expressions are expressions where project identifiers are combined via
binary operators. Parenthesis can be used to enforce operator precedence. There
are three operators:

 * `p = a + b` - Sum: `p` is executed when `a` or `b` is executed.
 * `p = a x b` - Product: `p` is executed when `a` and `b` is executed.
 * `p = a -> b` - Sequence: `p` is executed when `a` and `b` is executed, in order.

Please note that a equal sign (`=`) can be placed optionally just before the
definition of the expression.

#### Attributes

Following is a list of supported attributes of projects:

| Property name | Expected Type | Description |
|---------------|---------------|-------------|
| title         | text          | title of the project |
| description   | text          | longer description of what the project is |
| url           | URL           | reference in the web for more context about the project |
| owner         | username      | name of the person responsible for execution |
| progress      | percentage    | how much progress has been made so far (default 0%) |
| cost          | number        | estimated cost (default 0) |
| trust         | percentage    | probability of success (default 100%) |

Attributes can be specified between brackets, like, _e.g._:

```
b {
  title "build"
  description "our technology can be built and scale"
} phase1 -> phase2 -> phase3;
```

Or, optionally, if only "title" is define, as a single string literal, as _e.g._:

```
approvalProcess "approval process" legal -> budget -> executive;
```

There are "atomic" attributes that should be defined only for projects without
expressions: "cost", "trust", and "progress". Defining them and also expressions
is an error.

Example of atomic project:

```
sb {
  title "supplier B"
  trust 60%
  cost 5
  url "www.supplier.b.com"
  owner "partnerships"
};
```
