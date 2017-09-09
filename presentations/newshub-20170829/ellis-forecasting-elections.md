---
title: "Forecasting the election"
author: "Peter Ellis"
date: "22 August 2017"
output: html_document
---


## Forecasting the New Zealand election

### Living with uncertainty

In the wake of 2016's high profile voting surprises with Brexit and Donald Trump, some people have been asking if political polls have anything to tell us any more.  It's true that pollsters sometimes get it wrong.  For example, some of the key state-level polls in the USA in 2016 were found to be making the mistake of [not taking education into account in weighting their samples](https://www.nytimes.com/2017/05/31/upshot/a-2016-review-why-key-state-polls-were-wrong-about-trump.html?_r=0).  In previous years this hadn't caused too many problems, but in the changing political environment of 2016 it was a killer and was a big contributor to the underestimation of Trump's support in what turned out to be swing states.  But this is an easily fixable problem.

What about in New Zealand?  Let's look at polls over a longer period than normally shown, to find out the pollsters' record here.  In the chart below, the black circles show election results and the coloured lines represent opinion polls.

![](all-polls-and-years.svg)

In this first chart we can see that all told, the 12 pollsters that have had a go in public for various clients over the past 15 years haven't done too badly.  There's been a bit of a systematic underestimate of New Zealand First support perhaps, and maybe in the last couple of elections an overestimate of the Greens vote, but it's not too bad.

The other thing we see in that chart is that voting intention obviously changes over time.  This is something that's often forgotten when all the attention is on just the latest number.  But look at the 2004 surge in National's support after [Don Brash's controversial "Orewa" speech](https://en.wikipedia.org/wiki/Orewa_Speech), followed by a partial collapse to not much more than it had been six months earlier.  Or the interesting high levels in National support in the year or so before the 2008 and 2011 elections followed by swift movement down in the lead up to the election.  Or Labour's journey between 2011 and 2014 - growth in support up to solidly in the 30% range before a collapse in the last year to the election.  In any of these cases, taking an average of an individual pollsters' results over time and treating it as a prediction of the election day result is just unfair.

So, there's more to interpreting polls than just looking at one number and its margin of error.  In fact, there are three sources of uncertainty in translating a poll result into a prediction of the election:

- uncertainty from polls' relatively small sample size, which is published as the margin of error of "plus or minus two or three percent" and is the only indication of uncertainty that is usually given prominence
- the rest of what is known to statisticians as "total survey error", which comes from things like systematic small under- or over-estimates from subtleties in the polling companies' methods, or ways that sampling people in reality doesn't resemble just picking golf balls from a bucket (for example, unlike the golf balls, certain types of people may systematically refuse to talk to surveyers)
- even if we knew for sure how people would vote today, people change their minds right up to the last minute.

The 2017 UK election saw a clear and dramatic case of this latter challenge - in the short campaign, support for the Conservative and Unionist Party (the full name of the Tories) dropped rapidly.  And earlier in 2016, failing to predict the success of both Brexit and Trump was much more pundit failure than polls failure.  The polls weren't far off in either case (with some identifiable problems as mentioned above), but pundits treated them as though only the sampling error mattered, and (in the case of the USA) ignored some of the complexities in translating nation-wide estimates of vote into actual election results.  Hence they greatly underestimated the true uncertainty associated with voting day.

## Forecasting under uncertainty

In the lay mind, statistics is sometimes associated with precision (nerds with pocket calculators and lots of decimal places) and a need to know the exact truth.  The reality is that statisticians live in a world of fuzzy randomness and uncertainty, and more than anyone appreciate that ["we do not know, we can only guess"](http://www.azquotes.com/quote/908004) and ["all models are wrong, but some are useful"](https://en.wikipedia.org/wiki/All_models_are_wrong).  Statistics gives us tools to mitigate the uncertainty and quantify it.  

One of the best forecasts of the 2016 US election was Nate Silver's, which on the eve of the election [gave Donald Trump a 29% chance of winning](https://projects.fivethirtyeight.com/2016-election-forecast/) - in contrast to rival predictions of less than 5%.  With the pundits convinced that Trump was unelectable, Silver was criticised by many political commentators at the time.  That hasn't aged well for them... Read [this interesting story from 5 November 2016](http://www.politico.com/story/2016/11/nate-silver-huffington-post-polls-twitter-230815), just before the election, with Silver unloading on the "idiotic and irresponsible" forecasters at the Huffington Post who had Clinton at 98.3 percent chance of winning.  In fact, it seems likely that the over-confidence of the pundits in (mis-)interpreting the polls was a contributor to the eventual result, with a probable (but unprovable) demotivating impact on turnout.

We can use statistical methods to try to deal with the three types of uncertainty I listed earlier for the New Zealand election.  New Zealand's system is easier to work with than many countries because the country-level party vote is by far the most important factor.  Unfortunately, we also have much less polling to work with.  Here's my best predictions of the chances of various outcomes of the election on 23 September:

![](../../output/gam-final-chances-bar.svg)

That chart suggests the chances of the current National-led Coalition are a bit below one in three; and nearly all the remaining chances are for New Zealand First to have the balance of power.  Those probabilities come from a model that performs the following steps:

- It predicts each previous election result for each party by fitting smooth curves to polling companies' estimates.  Then those predictions are compared to the *actual* election results to get estimates for the systematic bias or "house effect" for each polling company for each party.
- The polls since 2014 are adjusted for those house effects, and smooth curves fitted to those polls and extended out, to predict the "expected" value of voting intention on 23 September
- Statistical methods are used to predict the uncertainty associated with that "expected" value, and simulations created of thousands of different party votes on the day
- Combined with assumptions about the key electorate votes for the Māori seats and for Epsom, those simulations are converted into a distribution of outcomes for seats in Parliament.

Here's the predictions of vote after correcting for the house effects:
![](../../output/gam-vote-predictions.svg)

We see that this method is fairly conservative.  There have been a couple of recent big polls for Labour.  Past experience is that this sort of bounce is temporary, but occasionally it leads to a level shift up.  Another few polls will tell us.  In the meantime, the uncertainty  translates into a wider range of predictions for Labour, and more uncertainty in general.

Here's how that expected vote, plus the uncertainty of election day, translates into the distribution of seats for various parties and combinations of parties:

![](../../output/gam-final-chances-histogram.svg)

Rather than choose all the coalition combinations myself, I made an [interactive web tool](https://ellisp.shinyapps.io/nz-election-2017/) in which you can make your own coalition combination and play around with the electorate seat assumptions.  For example, if you push Labour's chance of winning the Māori seats up to 0.95 for all seven (the highest allowed by the tool), the chance of the Labour-Māori-New Zealand First combination having enough seats to form a government goes to 0.76 (compared to 0.54 on the default assumptions); on the other hand, if you hand them all over to Māori or Mana party that chance goes down to 0.39.

The test of something like this is how it goes in predicting previous elections.  I fed my model the data that would have been available six months before the 2014 election and it predicted an 80% chance for National to win, with a slim possibility of a win by themselves and most of that chance made up of the win in coalition with partners, which turned out to be the result.  I'm pretty happy with that indicating that I've got at least a fair chunk of the three sources of uncertainty under control.  

Perfect, no; useful, I hope so.  One thing we can be sure of, there is more uncertainty than most people think, and much of that uncertainty is not about the polls but about the difficulty of predicting the future.  If anyone claims to be certain about this election, they're wrong.  Even if they turn out to be right.  Isn't probability interesting?

There's more information on [my personal website](https://ellisp.github.io/elections/elections.html) , including an alternative Model B that is more responsive to the latest polls; lots more different types of votring behaviour analysis; and links to the source code.