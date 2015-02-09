# NAME #
fddp - finds files with equal content

# SYNOPSIS #
```
fddp [files/patterns]
```

# DESCRIPTION #
fddp is simple shell tool that finds files with equal content in a given set of files. This is useful for finding duplicates in a set of documents where the same document might have been stored by accident with different names.

Given a list or wild-card pattern fddp analyses the files and prints a list of unique files in the list or an error message. For example given the two identical files example1.txt and example2.txt and the third different file example3.txt we would expect the following result:

    fddp example1.txt example2.txt example3.txt

    example1.txt
    example3.txt
    Right ()

Where "Right ()" indicates successful completion without any exceptions.

# AUTHOR #
Nikita Timofeev

# COPYRIGHT #
Copyright (c) 2011 Nikita Timofeev

