# Classic function declaration form
function main()
    println(hello())
end

# Compact function declaration form.
# Takes one argument with a default value.
hello(target="world") = "Hello, $(target)!"


# By default, this script runs the main function
main()
