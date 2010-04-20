Introduction
============

This is a [maxima] source code to verify the calculations presented in the
appendix of

> Aditya Mahajan, “Optimal decentralized transmission policies for two-user
> multiple access broadcast”, in Proceedings of the 2010 conference on decision
> and control (CDC).

Files
=====

The main source file is `value-function.txt`. It is written in [markdown]; the
embedded source code is in [maxima]. I prefer to write [literate] code, but
maxima does not provide[^1] any literate programming environment. So, I ended up
with this amalgam of markdown and maxima. 

The source code snippets are extracted using `extract-code.hs`, which is a
[Haskell] file that uses the [pandoc] programming API. 

The "tangled" source code is in `value-function.mac`. The corresponding maxima
output is `result.txt`. 

An advantage of writing literate code is that we can easily get the
prettyprinted output. The pdf output is `value-function.pdf` and the html output
is `value-function.html`[^2].




[maxima]: http://maxima.sourceforge.net/
[markdown]: http://daringfireball.net/projects/markdown/
[literate]: http://www.literateprogramming.com/
[Haskell]: http://www.haskell.org/
[pandoc]: http://johnmacfarlane.net/pandoc/

[^1]: Actually, maxima was designed for user interaction, and sometimes it goes
out of its way to make it hard to write scripts. But that is another story.

[^2]: I have not worked on a pretty css for the html output
