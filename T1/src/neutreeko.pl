:- consult('display.pl').
:- consult('ai.pl').
:- consult('logic.pl').
:- consult('game_modes.pl').
:- consult('utils.pl').
:- consult('menus.pl').

:- use_module(library(lists)).
:- use_module(library(system)).
:- use_module(library(random)).

/**
 * Entry point for the game. Prints the main menu, reads user's choice and redirects to
 * the given option.
 */  
play :-
    print_main_menu,
    write('Choose an option '),
    read(Option),
    choose_option(Option).