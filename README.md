#Super Source Config

Super Source Config is an extension of Dota2's config-file scripting language
that is aimed at making it easier to write complex, stateful, autoexec scripts.
Its main features are:

* The `bind` command accepts combinations of keys instead of just a single key.
* You can write stateful commands (such as toggles) using explicit variables
  and conditional statements instead of setting and resetting a bunch of aliases.
* A macro system that is an alternative to copy-pasting in many situations.
* Block syntax with `{}`. When an `alias` or `bind` command calls a list of
  commands, in scfg we can write these commands one per line, between braces.
  In the native cfg language we would have to write everything in a single line,
  surrounded by quotes and separated by semicolons.

All with a familiar and easy to learn syntax. Interested? Lets take a closer look!

##How to use scfg

First of all, go to my [github releases page](https://github.com/hugomg/scfgc/releases) to download the executable for the scfg compiler.

In Windows explorer, drag your scfg source file on top of the icon for the scfgc.exe executable.
scfg will convert that scfg file into a cfg file of the same name and place it next to the scfg source file.
You can then add a line on your autoexec.cfg file to call your script when Dota2 starts up.

For more complex scripts, scfg might not be able to compile your scfg file into a single cfg file.
In these cases, it will also generate a folder with the same base name as your scfg file, containing
helper files for your script.

Alternatively, you can invoke scfgc from the command line. Run `scfgc.exe --help` for usage instructions.

##Commands

A command consists of a command name followed by zero or more arguments. The argument list extends
until the next newline.

    echo hello world
    dota_disable_range_finder 0
    exec "foo/bar.cfg"
    dota_camera_set_lookatpos -2273 1800

You can also use semicolons to put more than one command in the same line:
  
    echo "first line"; echo "second line

Command names and arguments can be quoted or unquoted. Quoted arguments are written between double
quotes and may contain any characters other than the quote character itself. There is no mechanism
for representing special characters using backslashes.
Unquoted arguments can contain only numbers, letters, underscores and the + and - signs. This is a
difference from the native cfg language, where almost every character can appear outside of quotes.

###Comments

Comments begin with two forward slashes and continue until the end of the line.

    //This is a comment
    
There is no syntax for multi-line comments.

##Keybinds

The `bind` command creates new keybinds. It receives a key name and a command and sets things up
so that the command is run whenever you press that key.

    bind Q dota_ability_execute 0

Unlike the `bind` command in the original cfg language, the callback command doesn't need to be
written between quotes.

If you want to run more than one command, use braces to group them:

    bind "Z" {
      dota_ability_autocast 0
      dota_ability_autocast 1
      dota_ability_autocast 2
      dota_ability_autocast 3
      dota_ability_autocast 4
      dota_ability_autocast 5
    }

Another feature of scfg is that you can bind commands to combinations of keys

    bind "SPACE Q" dota_ability_quickcast 0

This keybind makes it so that when the Q key is pressed, if the space key is also being pressed
then it quick casts our first spell, instead of regular casting it as our other "Q" keybind did.

In theory you can use any key as a the trigger or modifier, except for SHIFT and CTRL, which
are hardcoded by Dota 2. As for ALT, you can use it if you want but to do so you must issue a
`dota_remap_alt` command somewhere in your autoexec.

     // This changes the key used for pings to "`",
     // freeing the ALT key for custom keybinds.
     dota_remap_alt "`"


Finally, you can bind commands to be run when a key is released similarly to how its done in the
native cfg language:

    bind "SPACE TAB" +showscores

If the command name of a keybind callback starts with a "+" then that command is run when the key
is pressed down and the "-" version of the command (`-showscores`, in this case) is run when the key
is released. scfgc takes special precautions to make sure this works correctly if you have composite
keybinds (this is something people often get wrong when coding by hand)

###Keybind caveats

1. Keybinds must be defined at the toplevel of your program and are immutable once defined. You
   cannot define new keybinds from inside another keybind's callback or the body of an alias.
2. Keybinds defined in the Dota GUI interface take priority over the keybinds defined in any config
   files. If you want to use a key in a config file keybind then you must remove any conflicting 
   keybinds from the settings GUI (you do this by selecting the conflicting keybind and right-clicking
   when the game asks what key to use).
3. The config file assumes that it completely "owns" any keys in its keybinds. If you have a scfg
   file binding on "SPACE Q" then you can't simultaneously have a separate config file that binds
   to "SPACE W" because one file will overwrite the SPACE bind from the other file.
4. Keybinds defined in a config file don't show up next to your ability and item icons when playing
   so its easy to forget what button is for what in the middle of the game. One workaround you can do is
   bind all items and skills to "ALT+X" combinations in the settings GUI. If you remapped ALT 
   using the `dota_remap_alt` command then those will actually mean "`+X", which doesn't conflict
   with the keybinds from the config file.

   
##Aliases

Aliases associate a name with a list of commands. The syntax is similar to the one for `bind`,
accepting either a single command or a list of them between braces:

    alias upgrade_courier {
      toggleshoppanel
      shop_nav_to_tab 0
      shop_select_itemrow 8
      toggleshoppanel
    }
    
    bind "ALT F5" upgrade_courier

Aliases in scfg are lexically scoped and are only visible from the point where they are defined
up to the end of the code block (}). This means that our program can have multiple aliases with
the same name, as long as they are in different scopes:

    {
      alias foo echo hello
      bind Q foo
    }
    
    {
      alias foo echo world
      bind W foo
    }

This feature is very important for the macro system we will see latter, but also means that 
during compilation scfgc translates all alias names to some automatically generated name such
as "a_autoexec_17". If you want to define an alias in one file and use it in another than you must
define it in a native cfg file instead of an scfg one.

Another difference from native aliases is in that scfg aliases are immutable, similarly to how
binds are immutable. If you try to redefine an alias what you will actually end up doing is 
creating a separate alias definition that shadows the original definition in the inner alias' scope.
If you want to change an alias' behavior, the way to do it is with variables, which we cover
in the next section.

##Variables

Sometimes, we want to make our keybinds stateful. For example, consider a keybind where the
first time you press it it moves the camera to the top rune and the second time it
moves the camera to the bot rune. In the native cfg language, the only  way to do this is
by writing a spaggetti ball of aliases that redefine each other when they are called, which
is tricky to write and very error prone.

The scfg approach to this is to make aliases immutable and use explicit variables for the sateteful
stuff:

    // Pressing F1 alternates between checking the top and bottom rune.
    bind F1 {
        var x :{TOP,BOT}
        switch x {
            TOP -> {dota_camera_set_lookatpos -2273 1800; x := BOT}
            BOT -> {dota_camera_set_lookatpos 3035 -2350; x := TOP}
        }
    }

Variables in scfg are lexically scoped, just like aliases, and always have a finite enumeration
of values they accept. You can use a `switch` statement to write commands that do different things
depending on the value of a variable and assignment statements with `:=`to change the value of a variable.

By default, variables are initialized with the first value in their enum (in the previous example, `TOP`)
but you can write an explicit initializer if you want.

    var x:{TOP, BOT} := BOT

###Increment statement

The "toggling" operation we did in the rune-camera keybind is very common so there is special support
for it with the increment primitive.

    bind F1 {
        var x :{TOP,BOT}
        switch x {
            TOP -> dota_camera_set_lookatpos -2273 1800
            BOT -> dota_camera_set_lookatpos 3035 -2350
        }
        increment x
    }

###Events

Another feature with variables is "events", which let you associate a command to be run whenever a variable
changes value. For, example these commands toggle the visibility of the network graph:

    var ng : {ON, OFF}
    when ng {
        OFF -> net_graph 0
        ON  -> net_graph 1
    }
    bind F10 increment ng

When the program first runs, `ng` is `ON` and the `net_graph 1` command will be run. Then, whenever
we press F10 the value of `ng` will change and the event handler will run the corresponding command.

Similarly to the `bind` command, `when` commands can only be issued at the toplevel of the program.

##Constant definitions and Macros

The scfg language has a simple macro system to reduce the amount of copy-pasting needed to write config files.
For example, consider the case where we want to bind a spell to Q, the quick cast version of that spell to
"SPACE Q" and the self cast version to "ALT Q". One way to do that is to be explicit and write the "Q"
in all 3 keybinds.

    bind Q {dota_ability_execute 0}
    bind "SPACE Q" {dota_ability_quickcast 0}
    bind "ALT Q" {dota_ability_execute 0; dota_ability_execute 0}

The downside of this is that if we want to change our spell keybind from Q to something
then we have 3 places to edit! What scfg lets us do instead is use a `define` statement to create a 
constant referring to what key to use as a keybind:

    define key Q
    bind $key {dota_ability_execute 0}
    bind "SPACE $key" {dota_ability_quickcast 0}
    bind "ALT $key" {dota_ability_execute 0; dota_ability_execute 0}

The way this works is that any dollar signs that appear in command or argument names are
converted to the value of the corresponding definition before the command is run.

In addition to using define to give a name to a string constant, we can also define macros,
which are kind of like aliases, but with parameters.

    define spell(key, id) {
        bind $key {dota_ability_execute $id}
        bind "SPACE $key" {dota_ability_quickcast $id}
        bind "ALT $key" {dota_ability_execute $id; dota_ability_execute $id}
    }
    spell(Q, 0) 
    spell(W, 1)
    spell(E, 2)
    spell(T, 3) //optional spell 1
    spell(G, 4) //optional spell 2
    spell(R, 5) //ultimate

In this example, we are defining 18 different keybinds using only 6 lines, without ever having to
copy-paste and repeat ourselves. If we decide to change the keybinds from QWER to ASDF there is only
one place we need to change.

##A taste of everything

This final example illustrates something that would be very tricky to write by hand
without the scfg language to help. Its a helper for culling blade HP markers that works in a
"radio button" fashion. If you press "ALT SPACE N" it changes the hp bar separator to match 
a level N axe ultimate and if you press the same button again it goes back to the default 
hp bar separator that you use when you are not playing axe.

The "_" in the switch statements is a wildcard pattern that matches any value. It acts as a
"default" or "else" case.

    var hp : {DEF, LVL1, LVL2, LVL3, AGHS1, AGHS2, AGHS3}
    
    define hpbars(n){ dota_health_per_vertical_marker $n }
    
    when hp {
        DEF   -> hpbars(200)
        LVL1  -> hpbars(250)
        LVL2  -> hpbars(350)
        LVL3  -> hpbars(450)
        AGHS1 -> hpbars(550)
        AGHS2 -> hpbars(550)
        AGHS3 -> hpbars(650)
    }
   
    bind "ALT SPACE 1"  switch hp { LVL1  -> hp := DEF; _ -> hp := LVL1  }
    bind "ALT SPACE 2"  switch hp { LVL2  -> hp := DEF; _ -> hp := LVL2  }
    bind "ALT SPACE 3"  switch hp { LVL3  -> hp := DEF; _ -> hp := LVL3  }
    bind "ALT SPACE 4"  switch hp { AGHS1 -> hp := DEF; _ -> hp := AGHS1 }
    bind "ALT SPACE 5"  switch hp { AGHS2 -> hp := DEF; _ -> hp := AGHS2 }
    bind "ALT SPACE 6"  switch hp { AGHS3 -> hp := DEF; _ -> hp := AGHS3 }
   
##Future improvements

scfg is still very experimental so any suggestions and comments are very welcome!
