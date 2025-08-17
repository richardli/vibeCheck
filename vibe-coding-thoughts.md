## Vibe-coding an R package: fun until it's not

This was my first vibe-coding project: forcing myself to code only through prompts, without diving into function details even when I knew how to fix them. It ended up taking a good portion of small pocket time across four days... Why am I doing this? I’m sure vibe-coding is picking up steam among professional programmers, but for students in statistics and data science, there isn’t much guidance out there. Definitely no textbook :) So I figured why not. I can’t judge it unless I seriously try it, so here it goes.

TL;DR: it worked. But I deliberately tried a process not fit for vibe-coding, and it indeed failed the bigger task. What I am going to tell students? Use AI to help you code, definitely, but don't do it blindly.

I used Claude Sonnet 4 as my coding partner. A vibe choice, for sure. Other LLMs may behave differently, but broadly speaking, the experience would be similar. My task was simple: clean up an R package our group had been building fast without careful documentation. I already knew how to get the job done, either hours of boring copy-pasting or a regex-heavy bash script. Instead, I thought: why not make an R package that helps me clean up R packages?

In the pre-AI dark era, this may be something that's perfect for a student project (poor soul!). It could be a useful project, and could be a good learning experience for students. But it's not a great project since it's very software engineering focused, and not super useful for stat/DS students. (Note: whether it's actually useful is debatable, but I guess not useful in terms of the conventional criterion we use to evaluate stats/DS students.)  

Alright, so I start chatting with Claude. At first, it was great. Claude quickly spun up a Shiny dashboard to visualize package structure in minutes. A supercharged student went into the most hairy task without hesitation, Claude understood the most tedious and vague tasks immediately and produced polished solutions.

But then the dark side showed up. Functions became long, unreadable, and fragile. Debugging turned into a cycle of copy–paste–prompt–repeat. What should’ve been a 10-minute manual fix ballooned into an hour-long back-and-forth. Claude is more thoughtful than me (or any student) at anticipating edge cases, but many of those edge cases were things that would almost never happen in real R code.

There are some things I learned through reading Claude's codes too, e.g., R6. But overall, the system Claude set up becomes too heavy to realistically handle, even by the AI. Small changes can trigger massive function updates and I quickly hit rate and chat length limits. Eventually, I had to reset. I asked Claude for a restructuring: drop R6, rebuild as task-specific functions, etc. Then things become at least a bit more manageable. Even though I had to break my original plan and provide more specific prompt for code implementations inside functions to Claude, it starts to click a bit better...

Long story short: I got everything I wanted done, and built a tool I might even reuse. From the outside it looks polished and well-documented. Inside… who knows. Some bugs are still lurking, but at this point I don’t really need them fixed. I cleaned up the README just enough for future me, and wrote this piece...

A few thoughts after the vibekathon:

- _Will I use AI to vibe code in the future?_ Definitely. If I was trying to only solve my original problem at hand without creating a slightly more general tool, it could be done super fast. There are many situations in academic research that involves `dirty work' that needs to be done, but doing it manually takes about the same time as writing an automatic script from scratch. Now with AI, such utility scripts can be written instantly. This is especially useful for tasks where manual checking the correctness of the output is fast (so less concern about accuracy/hallucination).

- _Will I use AI to vibe-code a large project?_ Probably not, at least not with my current vibe-coding skills. Many things can be done better than this attempt of course. For example, I can have a clear design of the project and let AI implement specific pieces instead of letting it freestyle. I may save myself a lot of trouble by using an actual AI agent that can implement the codes and iterate based on outputs. I may benefit by watching a few youtube vibe-coding tutorials than starting blank, etc. But after all, for a large project that I care about, I will need to be able to read and maintain the code base. I will want to implement methods and functions using the styles and habits that I am most comfortable with so that I can continuously maintain them, even after not looking at them for a long while. Of course, maybe AI tools will become better and can learn from how I code and mimic? It may also be useful to have AI rewrite my own old codes using my current style?

- _What should I tell students to do?_ I think we are at a point where banning AI is not an option. It will similar to requiring hand-written papers when computers exist. But the challenges I encountered are likely going to be experienced by students too. The concern is always whether students can realize these issues on their own? Or will they be simply happy that something is returned and seems reasonable? I think it is important for all students to try vibe-coding at some point, but engage with the AI assistant deeply to make sure they understand what is happening under the hood. I think AI does opens up a wonderful opportunity for students to think more about **designing** their codes rather than memorizing detailed syntax (relatively, knowing syntax is important of course!), especially for stats/DS students who are not traditionally trained in a coding-heavy way. Of course, the real challenge is how to convey all these to students, especially undergrads who are just starting. In some sense, this is probably more difficult than teaching students to write with a pen when they can type?

- I think for advanced/grad students, again, I'm thinking about stats/DS students whose job/research involves a lot of detective work, knowing how to vibe-code is important. Used well, it can save a lot of time and let you focus on reasoning and the parts that really matter, instead of being drowned in the tedious details of data investigation. Regardless of how you feel about AI, it’s here to stay, so why not make it useful for yourself?


PS: One last thing AI can do for me in this repository: these thoughts were polished by ChatGPT for language. I asked chatGPT to maintain my original tune and had to undo some of the optimistic spins after it's polishing. But if there is anything that you don't like, I'll blame ChatGPT! :)






















