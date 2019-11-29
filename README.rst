######
forsat
######

The ``forsat`` code base is a generic set of Fortran modules written to the latest standards of the
language.  The code was developed with numerical simulations of both aerospace and health policy
applications in mind.

It can be very difficult to find modern examples of Fortran code that weren't written before I was
born and take advantage of modularity, robust documentation, unit testing and object oriented
principles.  This code will hopefully provide good examples of how it can now be done within the
language.

With that being said, there are three remaining issues that make this language a poor choice. It is
not an retrospective language, so generic programming is basically impossible.  It can't intercept
exceptions, so you must either hard stop an application or return an error code that someone might
decide to listen to.  Third, Fortran remains absolutely pitiful at processing text, so it completely
fails in many areas of machine learning, and makes it very difficult to get inputs in and out of
simulations.

*****
Notes
*****

1.  First attempted by David C. Stauffer circa 2014 to write some quaternion and vector tools, plus
    binary file utilities.
2.  Mostly written by David C. Stauffer in September 2019 based on the existing Python ghap code, and
    the MATLAB hesat code to see if performance runtimes could be improved.
3.  Posted by David C. Stauffer in December 2019 to provide an example of what good modern Fortran
    code might look like with the hope that some of it will be incorporated in Redy in the future.
