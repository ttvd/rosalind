#!/usr/bin/env coffee
# Jake build file for Rosalind problems.

try
    require.resolve "jake"
    require.resolve "del"
    require.resolve "shelljs"
    require.resolve "fs"
    require.resolve "jsesc"
catch e
    console.error "Please install necessary modules."
    process.exit e.code

# We can load modules.
jake = require "jake"
del = require "del"
shelljs = require "shelljs"
fs = require "fs"
jsesc = require "jsesc"

# Default task.
desc "This is the default task."
task_default = (params) ->
    jake.showAllTaskDescriptions([]);
task "default", [], task_default

# Build problem.
desc "Build problem, jake build[dna] -- will build DNA problem."
task_build = (problem_name) ->
    jake.Task["build_common"].invoke();
    file_name = "rosalind_" + problem_name + ".erl"
    if fs.existsSync file_name
        build_result = shelljs.exec ("erlc " + file_name), {silent: true}
        if build_result.code != 0
            console.log "Failed building \"" + problem_name + "\" problem."
            console.log build_result.output
        else
            console.log "Successfully built \"" + problem_name + "\" problem."
    else
        console.log "Could not find \"" + problem_name + "\" problem. Please pass problem name as parameter."
task "build", [], task_build

# Run problem.
desc "Run problem, jake run[dna] -- will run DNA problem."
task_run = (problem_name) ->
    file_name = "rosalind_" + problem_name + ".beam"
    if fs.existsSync file_name
        run_result = shelljs.exec ("erl -noshell -s rosalind_" + problem_name + " -s init stop"), {silent: true}
        if run_result.code != 0
            console.log "Failed running \"" + problem_name + "\" problem."
        else
            console.log "Successfully ran \"" + problem_name + "\" problem."
        console.log run_result.output
task "run", [], task_run

# Build common utility module.
desc "Build common utility module, jake build_common -- will build common module."
task_build_common = (params) ->
    file_name = "rosalind.erl"
    if fs.existsSync file_name
        build_result = shelljs.exec ("erlc " + file_name), {silent: true}
        if build_result.code != 0
            console.log "Failed building common module."
            console.log build_result.output
        else
            console.log "Successfully built common module."
task "build_common", [], task_build_common

# Clean task.
desc "Clean all product files, jake clean -- will remove all intermediate files."
task_clean = (params) ->
    del ["*.beam", "*.dump"]
    console.log "Cleaning finished."
task "clean", [], task_clean
