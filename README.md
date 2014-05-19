Minimal todo list written in Haskell.
=================

This todo list forces you to take action on the top todo before you can move on to the next todo.
Taking action could mean either completing the task or moving it to a different context. For 
example, if you are at work, but this is a todo that can only be done at home like washing the
dishes, just move the todo to the 'home' context and keep going through your todos until you've
completed all the ones at work. Once you are home, you go through the home todos.

Display the todo list

    ./main show
    
Add a new todo

    ./main add "task description"
    
Complete the first todo (at the top of the display list). We enforce this so that you 
must take action on the top todo. This could simply mean moving it to another context.
Completing the item deletes the todo forever.

    ./main complete
    
Move the first todo to a different context and move it to the bottom of the list. 
Possible contexts: home, work, shopping, none.

    ./main move home
    
Building
====

    ghc --make main
    
