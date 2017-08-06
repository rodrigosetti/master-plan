# master-plan

Master Plan is a text based project management tool that implements an
algebra of projects.

These are the values propositions of master plan:

 * Simplicity: keep project management into a single text file.
 * Agility: embrace change, by allowing projects to specify uncertainty and allow
   for refinement anytime.
 * Freedom: master plan is a open specification, not dependent on tools or hosting.
   There is this current open-source implementation, but anyone can implement
   tools or visualizations on top of it.

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

The entire definition of a project is defined into a single `plan.txt` file
using a simple language. There are defaults for most constrains and properties
such that things can be less verbose if using the defaults.

The tool is able to build visualizations and reports from the plan file.

Ideally, the plan file should be kept in version control so that execution and
planning progress can be recorded.

### Commands

The `mp` command line tool supports the following commands:

 * `prioritize` - list, in order of priority, the projects ready for execution.
 * `render` - generate a report output, specified by one of the backend formats.

### Syntax

Comments are preceded by hashtag (`#`), and extend to the end of line
(like Shell and Python).

Everything else are definitions, in the form `lrs = rhs`.
There are two kinds of definitions with respect to `lrs` (left hand side):

 * Definition of a project: in the form `identifier = expression`
 * Definition of a property of a project: in the form `identifier(identifier) = expression`.
   This is used to define properties of names.

A project is identified by a unique identifier. The "root" project is identified
by a special `root` identifier.

Project expressions are expressions where project identifiers are combined via
binary operators. Parenthesis can be used to enforce operator precedence. There
are three operators:

 * `p = a + b` - Sum: `p` is executed when `a` or `b` is executed.
 * `p = a x b` - Product: `p` is executed when `a` and `b` is executed.
 * `p = a > b` - Sequence: `p` is executed when `a` and `b` is executed, in order.

#### Properties

Following is a list of supported properties of projects:

| Property name | Expected Type | Description |
|---------------|---------------|-------------|
| name          | text          | title of the project |
| description   | text          | longer description of what the project is |
| url           | URL           | reference in the web for more context about the project |
| owner         | username      | name of the person responsible for execution |
| status        | blocked,ready,progress,done,cancelled   | status of execution |
| progress      | percentage    | how much progress has been made so far |
| cost          | number        | estimated cost (aliases: "time", "estimation") |
| risk          | percentage    | risk of failure |

#### Grammar

```
definition = project_def | predicate_def

project_def = identifier "=" expression
expression = term ((">>" | "x") term)*
term = factor ("+" factor)*
factor = "(" expression ")" | identifier

predicate_def = identifier "(" identifier ")" "=" value
value = number | text
```