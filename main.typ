#import "@preview/xarrow:0.3.1": xarrow
#import "@preview/gentle-clues:1.0.0": tip, clue, tip
#import "@preview/whalogen:0.2.0": ce
#import "@preview/ilm:1.2.1": ilm

#let correction(title: "Correction", icon: emoji.face.inv, ..args) = clue(
  accent-color: red,
  title: title,
  icon: icon,
  ..args
)
#let caution(title: "Caution", icon: emoji.warning, ..args) = clue(
  accent-color: orange,
  title: title,
  icon: icon,
  ..args
)
#let alternate(title: "Alternate Approach", icon: emoji.face.explode, ..args) = clue(
  accent-color: blue,
  title: title,
  icon: icon,
  ..args
)
#let plot(title: "Interactive Plot", icon: emoji.chart, ..args) = clue(
  accent-color: purple,
  title: title,
  icon: icon,
  ..args
)
#let tip(title: "Helpful Tip", icon: emoji.brain, ..args) = clue(
  accent-color: yellow,
  title: title,
  icon: icon,
  ..args
)
#let self(title: "Note to Self", icon: emoji.mirror, ..args) = clue(
  accent-color: gray,
  title: title,
  icon: icon,
  ..args
)
#let std = sym.circle.small
#let conc(val) = $[ce(val )]$
#let rad = sym.circle.filled.small
#let eqArrow = xarrow.with(
  sym: sym.harpoons.rtlb,
  margin: 0.5em
)
#let fwdArrow = xarrow.with(
  sym: sym.arrow.r,
  margin: 0.5em
)
#let ddagger = sym.dagger.double
#let delplot(rank) = $""^rank"P "_"A "$

// #set math.equation(numbering: "(1)")

#show: ilm.with(
  title: [Chemical Reaction Engineering],
  author: "Andrew S. Rosen",
  abstract: [Lectures notes for a graduate-level course.],
  paper-size: "us-letter",
  preface: [#align(center + horizon)[Copyright #sym.copyright 2024 Andrew S. Rosen.

Licensed under the Creative Commons Attribution-NonCommercial-ShareAlike 4.0 License (the "License"). You may not use this file except in compliance with the License. You may obtain a copy of the License at https://creativecommons.org/licenses/by-nc-sa/4.0.

A.S.R acknowledges Prof. Aditya Bhan, Prof. Linda Broadbelt, and Prof. Justin Notestein for inspiration on various topics covered within this course.
Most importantly, A.S.R. is grateful for the thoughtful questions, comments, and corrections provided by his students who have helped shaped this text during their journey.

This document was typeset using Typst (https://typst.app).]
],
)

= Setting the Stage <setting-the-stage>

== Course Perspective

Before we set sail on our journey, it is probably wise to state upfront what this course does --- and does not --- entail.
After all, it would be a bit difficult to appreciate the journey if you have no idea about the destination.

This course is specifically designed to provide a foundational understanding of how chemical reactions proceed and how to engineer them in ways that are ultimately beneficial for the chemical engineer.
Given the extremely diverse and increasingly interdisciplinary areas of research that the modern chemical engineer can pursue, this course focuses on topics that are likely to be of widespread relevance to researchers.
This, primarily, translates to a significant focus on reaction kinetics.

As a field of study, reaction kinetics is fascinating from a metascience perspective.
In my somewhat biased opinion, there is no field of study in the chemical engineering curriculum that is more prone to misconceptions and questionable assumptions.
Part of the reason for this pedagogical challenge is that kinetics can be both a theoretically rigorous _and_ a purely empirical science depending on how one approaches the topic (naturally, we will lean more towards the former in this course).
After all, if the rate coefficient $k$ should actually be $4 k$, this is of no major concern if one treats kinetics as merely an exercise in statistical regression, but it can influence the meaning of the underlying kinetic parameters if treated from a theoretical basis.

Perhaps a more notable cause of pedagogical challenges in the reaction kinetics literature --- including both textbook and research articles --- is that the intersection of kinetics and thermodynamics can become fraught with logical inconsistencies.
Frequently, derivations in reaction kinetics are carried out under assumptions of thermodynamic ideality.
When these assumptions break down, it is tempting to re-introduce correction factors in a _post hoc_ manner, but doing so often yields deceptively incorrect mathematical expressions.
One of the most notable examples of this is the inclusion of activity coefficients in the transition state theory derivation.
The lack of attention dedicated to the proper accounting of standard state properties further complicates matters as well.

This course seeks to present reaction kinetics with as theoretically sound of a basis as possible, emphasizing rigor and precision at each step of the journey.
Additionally, nearly every derivation is carried out in full with no steps left as "an exercise for the reader," as this is often where errors are most likely to arise.
Interactive plots are presented throughout the text to allow the reader to engage more directly with the underlying equations when doing so would yield unique insights.

Of course, given the nature of limited time, there must always be tradeoffs.
In the design of this course, several topics of relevance to the practicing reactor engineer have been intentionally omitted or alluded to only in passing.
Nonetheless, the hopes is that this course will ultimately provide unique insights into the fundamentals of chemical reactions and their kinetics in a way that is broadly applicable to the modern chemical engineering graduate student regardless of their chosen area of study.
Rest assured --- there is plenty to learn and much left to be discovered!

== Why Reaction Engineering <chemical-reactors-in-society>
 
Chemical reactions and the reactor vessels in which they occur have a profound impact on society and life as we know it.
In this section, we will briefly highlight a few representative examples of chemical reactors that are relevant to our everyday lives, mainly to provide context for why it is worth pursuing the fundamentals of chemical reaction engineering in the first place.

=== The Sun <the-sun>

Before we discuss conventional chemical reactors designed by humans, it is worth taking a moment to appreciate that the fundamental principles of reaction engineering can be applied to an enormous range of systems that long predate an industrialized society.
To demonstrate this point, consider the Sun.
In many ways, the Sun can be thought of as a massive chemical reactor: it is a confined volume in which a chain reaction is continually taking place, providing the energy that sustains life on Earth via the conversion of hydrogen into helium.
While these reactions are nuclear rather than chemical, the same fundamental principles of reactor analysis apply.
The Sun is a reminder that the principles of chemical reaction engineering are not just relevant to industrial processes but to the very fabric of the universe itself.
Of course, one could go even further to argue that the Big Bang itself is the largest reaction of all with our universe as the largest reactor, but that is a discussion for another time.

=== Cells and Enzymes <cells-and-enzymes>

Fast-forward many millions of years after the formation of the Sun, and we arrive at the evolution of life on Earth.
Cellular life itself is a testament to chemical reactors in action.
Surely, you may recall that the mitochondria is the powerhouse of the cell, and these miniature reactors are carrying out complex reaction cascades via the citric acid cycle to support cellular respiration.

As chemical engineers, we can draw significant inspiration from nature's chemical reactors. For instance, consider methanotrophs: a family of bacteria that primarily metabolize methane as their main source of chemical energy.
Methane itself is a greenhouse gas that, while in lower concentrations than carbon dioxide in the atmosphere, has an 80#sym.times higher global warming potential within the first 20 years of its release.
Chemical engineers have been inspired by methanotrophs to research ways to directly convert methane --- the primary component of natural gas --- into cleaner and more valuable chemical feedstocks.
Heterogeneous catalysts, for example, that mimic the structure of the active sites in methane monooxygenase enzymes found within methanotrophs have been shown to directly and selectively convert of methane into methanol.
While carrying out this process efficiently and cost-effectively at industrial scales remains a "holy grail", it is worth keeping in mind that the core principles of reaction engineering spans many disciplines that can each benefit from the insights of one another.

=== Ammonia Synthesis <ammonia-synthesis>

Continuing our highly abbreviated journey through time, we arrive at human civilization and, eventually, the dawn of the modern chemical industry as we know it today.
In the early 1900s, one of society's most important chemical reactors was developed: the ammonia reactor.
Powered by the Haber--Bosch process devised by Fritz Haber and Carl Bosch, the modern ammonia reactor is the main industrial mechanism for producing the ammonia that is used in nitrogen fertilizers.
The Haber--Bosch process itself constitutes the following net reaction: 
$ ce("N2 + 3 H2 <=> 2 NH3"). $

Colloquially referred to as "converting air into bread", this process produces the ammonia that is central to nitrogen fertilizers, a process so important to society that it is believed to have been a critical factor in the global population boom during the 20#super[th] century.
#footnote[
  For additional historical context about the Haber--Bosch process and the complicated history of Fritz Haber himself, watch "The Man Who Killed Millions and Saved Billions" by Veritasium: https://www.youtube.com/watch?v=QQkmJI63ykI.
]
In fact, it is estimated that up to half of the nitrogen atoms in the human body may have gone through the Haber--Bosch process.
We will revisit the ammonia reactor and the Haber--Bosch process several times throughout this course, as it is an excellent example that ties together many of the fundamental concepts of chemical reaction engineering.

=== Making and Leveraging Liquid Hydrocarbons <making-and-leveraging-liquid-hydrocarbons>

Fast forwarding to the 1930s, the rise of the automobile and increased reliance on fossil fuels led to several advances in reactor technologies.
One representative discovery was the catalytic cracker: a reactor that uses an aluminosilicate material known as Zeolite Y to catalyze the conversion of high-molecular weight, linear alkanes found in petroleum into smaller, more valuable hydrocarbons.
#footnote[If you ever find yourself driving back to Princeton from New York City, take a look in the westward direction near the entrance of Interstate 95. You will likely be able to see the Phillips 66 Bayway Refinery, which has a fluid catalytic cracking unit (among other chemical reactors). Roughly one out of every 20 ships in New York Harbor are bound for this refinery, where crude oil is distilled, cracked, and purified.]
Another relevant process pioneered during this time period is the Fischer--Tropsch process, which converts carbon monoxide and hydrogen (known as synthesis gas or "syngas") into liquid hydrocarbons.
#footnote[
  For a molecular-scale view of how the Fischer--Tropsch reaction works, watch the following animation from the Eindhoven University of Technology: https://www.youtube.com/watch?v=44OU4JxEK4k.
]
Historically, countries with substantial coal resources have benefitted the most from the Fischer--Tropsch process as a means of producing diesel fuels without relying on foreign imports of oil. 
The synthesis gas in this case is produced via coal gasification, wherein coal is reacted with oxygen, steam, or air at high temperatures.

=== Green Technologies <green-technologies>

While much of the history of chemical engineering is intricately tied to the fossil fuel industry, its future is far more diverse in scope.
Recently, there has been a significant push towards leveraging chemical reaction engineering principles to address the many clean energy and sustainability challenges facing our planet.
Some areas of interest include the design of fuel cells that convert hydrogen and oxygen into electricity, porous materials that can capture and convert carbon dioxide, and cutting-edge technologies to "upcycle" plastic waste into valuable commodity chemicals.
Even further into the future, one can think about circling back to the original reactor described in this section: the Sun.
While likely many decades away from commercialization, nuclear fusion reactors are being designed with the hope that they can one day serve as a clean and abundant source of energy for humanity.
#footnote[In fact, one of the earliest fusion reactor designs --- the stellarator --- was pioneered at the Princeton Plasma Physics Laboratory (PPPL) and set the stage for improved fusion reactor design both at PPPL and around the world.]

= Reaction Rates and Rate Constants <reaction-rates-and-coefficients>

== Generic Expressions for Chemical Reactions <generic-reactions>

=== Stoichiometry for Individual Reactions

We start by considering a generic chemical reaction given by the following expression:
$ alpha ce("A ") + beta ce("B ") --> gamma ce("C ") + delta ce("D "), $<eq:general_rxn>
where A and B are reactants, C and D are products, and $alpha$, $beta$, $gamma$, and $delta$ are the corresponding stoichiometric numbers to ensure that mass is conserved.
For now, we will assume that this reaction is an _elementary_ reaction, meaning that it is a single, well-defined reaction step.

It is often useful to think about chemical reactions in a more mathematically oriented framework.
For this, we will define $A_j$ as the $j$-th species taking place in the reaction (starting from $j=1$).
We will also define $nu_j$ as the stoichiometric coefficient of species $A_j$, wherein reactants have $nu_(j)<0$ and products have $nu_j>0$ by convention.

For our example reaction given by #ref(<eq:general_rxn>), we would have
$
bold(A) = mat(A_1, A_2, A_3, A_4)\
bold(nu) = mat(-alpha, -beta, gamma, delta),
$
where $A_1$, $A_2$, $A_3$, and $A_4$ are synonymous with species A, B, C, and D, respectively.

From this convenient set of mathematical notation, we can rewrite our chemical reaction as
$ -alpha A_1 -beta A_2 + gamma A_3 + delta A_4 = 0 $
or more generally as
$ sum_j nu_j A_j = 0, $ <eq:summation_stoichs>
the latter of which is true for any balanced chemical reaction.
#footnote[This can be written in matrix form as $bold(nu) dot.op bold(A)^"T " = 0$, where the superscript "T" indicates that the row vector is transposed to form a column vector.]

=== Stoichiometry for Multiple Reactions <stoich-multiple-rxns>

Naturally, the next step is to consider multiple chemical reactions taking place.
For instance, perhaps we instead have the following two elementary reactions:
$
alpha ce("A") + beta ce("B") -> gamma ce("C")\
gamma ce("C") -> delta ce("D").
$<eq:multiple_rxns>

Here, we will use $i$ to denote individual reactions in contrast with $j$ for distinguishing species.
In this case, we will define a two-dimensional stoichiometric matrix $nu_(i,j)$, which simply means "the stoichiometric number for the $j$-th species in the $i$-th reaction."
For our example, we have
$ 
bold(A) = mat(A_1, A_2, A_3, A_4)\
bold(nu) = mat(-alpha, -beta, gamma, 0; 0, 0, -gamma, delta).
$

For our example reaction, we have 
$ 
-alpha A_1 -beta A_2 + gamma A_3 = 0, quad -gamma A_3 + delta A_4 = 0.
$
In analogy with #ref(<eq:summation_stoichs>), we can write a general expression for a balanced set of chemical equations in nearly the same way:
$ sum_j nu_(i,j) A_j = 0, $
where the above expression is written for each reaction $i$.
#footnote[This can be written more compactly in matrix form as $bold(nu) dot.op bold(A)^"T " = bold(0)$.]

Throughout this course, we will mostly write out the systems of equations, avoiding the need for a linear algebra-based perspective.
However, the matrix representations were nonetheless included in our prior discussion to emphasize a different way of thinking about expressing chemical reactions.
The matrix forms are also convenient from a computational perspective, as they are a more compact and more efficient way to express the system of equations.

== The Rate of Reaction <the-rate-of-reaction>

=== Defining the Rate <defining-the-rate>

==== Rate of Change for a Species

With the generic reactions introduced in #ref(<generic-reactions>), we can ask the fundamental question: what is the rate of change of the $j$-th species in a given reaction, denoted $r_j$?
Intuitively, one might say that $r_(j)$ is simply the number of moles of $A_j$ being generated per unit time.
Most commonly, however, we will define $r_j$ in an intensive manner such that it has the following units:
$ r_j = ["moles of " A_j] / (["time"] dot ["reaction space"]). $<eq:rate_units>
Here, the reaction space is typically a representative volume used to normalize the rate so that it is an intensive property.
When the chemical reactor and its contents are spatially uniform, the reaction space can be the reactor volume itself; otherwise, it is limited to a differential, volumetric element in the reactor under consideration.

Written algebraically, the rate of generation for species $A_j$ can be given as
$ r_j equiv 1/V (dif n_j)/(dif t), $
where $V$ is the normalizing system volume, $n_j$ is the number of moles of the $j$-th species, and $t$ is time.
In practice, other normalizations for $r_j$ can be used in place of volume.
For instance, in catalytic reactions, it is oftentimes more convenient to normalize the rate on the basis of the surface area or weight of the catalyst that is driving the reaction.

#caution[
Note that the definition of $r_j$ is not the rate of change in species concentration, as that equality can only be made if there is a constant and uniform volume.
To demonstrate this point, let us define $[A_j]$ as the concentration of species $A_j$.
We can rewrite $n_j = [A_j] V$ to arrive at
$ r_j = 1/V (dif ([A_j] V))/(dif t) = 1/V (V (dif [A_j])/(dif t) + [A_j] (dif V)/(dif t)). $
If and only if the reaction volume remains constant, we have
$ r_j = 1/V (V (dif [A_j])/(dif t)) = (dif [A_j])/(dif t). $
]

For a system of several reactions, we will likely be interested in the _net_ rate of change in species $A_j$. 
This is nothing more than the sum of all the rates of change for species $A_j$ across each reaction.
Returning to the set of reactions given in #ref(<eq:multiple_rxns>),
$ alpha ce("A") + beta ce("B") -> gamma ce("C")\
gamma ce("C") -> delta ce("D"), $
we can define the individual rates of change for each species as
$
r_"A " &= r_(1,"A ")\
r_"B " &= r_(1,"B ")\
r_"C " &= r_(1,"C ") + r_(2,"C ")\
r_"D " &= r_(2,"D ").
$<eq:r_c_species>

More generally, the species rates of change can be expressed as
$ r_j = sum_i r_(i,j), $ <eq:sum_of_rxn_species>
where the summation is taken over each reaction $i$ in the reaction scheme.

==== Rate of a Reaction

===== In Relation to Rates of Change for Species 

Continuing our terminology adventure, we can ask: what is the rate for the $i$-th reaction (instead of the $j$-th species)?
We will define the rate of reaction, $r_i$, to be proportional to the rate of generation or consumption of each species based on the corresponding stoichiometric coefficients.

For instance, the rates of the two reactions in #ref(<eq:multiple_rxns>),
$ alpha ce("A") + beta ce("B") -> gamma ce("C")\
gamma ce("C") -> delta ce("D"), $
can be written out as
$
r_1 &= -r_(1,"A ")/alpha = -r_(1,"B ")/beta  = r_(1,"C ")/gamma \ 
r_2 &= -r_(2,"C ")/gamma  = r_(2,"D ")/delta .
$
In mathematical terms, we can state more generally that for the $i$-th reaction and $j$-th species that
$ r_i equiv r_(i,j)/nu_(i,j). $<eq:stoichs>

Unlike the rate of change for a species, the rate of reaction is always a positive quantity, which is the motivation behind the sign conventions in the above expression.

With this formalism, we can rewrite the net rate of change for a given species across multiple reactions (originally presented in #ref(<eq:sum_of_rxn_species>)) in terms of the individual reaction rates.
For our example set of reactions, we would have
$
r_"A " &= -alpha r_1\
r_"B " &= -beta r_1\
r_"C " &= gamma r_1 - gamma r_2\
r_"D " &= delta r_2.
$<eq:r_change_example>
Take a moment to contrast the above expressions with #ref(<eq:r_c_species>), which was written in terms of the rate of change for each species rather than the rates of reaction.

More generally, we can express the rate of change for a given species as simply the sum of each reaction rate scaled by the corresponding stoichiometric coefficients.
In other words,
$ r_j = sum_(i) nu_(i,j) r_i, $<eq:sum_stoichs_rate>
where the summation is taken over each reaction $i$ in the reaction scheme.
#footnote[
  We can rewrite #ref(<eq:sum_stoichs_rate>) more generally in matrix form as
$bold(r)_"species"^" T" = bold(nu)^"T " dot.op bold(r),$
where $bold(r)_"species"$ and $bold(r)$ are the _net_ production rates of each species and the _individual_ reaction rates, respectively.
]

===== In Relation to the Extent of Reaction

Thus far, we have defined the rate of a reaction as being some property that is defined by the rates of change of each species, normalized in an internally consistent manner that is not dependent on the choice of species.
However, we can provide a slightly more proper definition if we introduce a new term: the extent of reaction.
The extent of a reaction, $xi$, is a measure of the reaction progress (typically in units of moles) based on the number of chemical transformations,
#footnote[For a reaction #ce("2A -> B + C") with an initial value of $n_ce("A")= 2 "mol"$ and final value of $n_ce("A") = 0.5 "mol"$, we would have $xi = -(0.5 "mol" - 2 "mol")\/2 = 0.75 "mol"$. This value of $xi$ could then be used to find the relative change in #ce("B") and #ce("C") based on their stoichiometric coefficients.]
typically defined as
$ dif xi equiv (dif n_j)/nu_j. $
With this, we can define the rate of the $i$-th reaction as
$ r_i = 1/V (dif xi_i)/(dif t). $
We can confirm that our definitions are internally consistent via substitution:
$ r_i = 1/V ((dif n_(i,j))/nu_(i,j))/(dif t) $
$ r_i = 1/nu_(i,j) 1/V (dif n_(i,j))/(dif t) $
$ r_i = r_(i,j)/nu_(i,j), $
which is identical to #ref(<eq:stoichs>).

At this point, it may feel like this is all just a dozen different ways to say very similar things.
This would not be a false statement!
It is important to be extremely precise when discussing reaction kinetics.
A "rate" can mean many different things depending on the context.

=== The Rate Function <mass-action-kinetics>

In general, we write the rate of reaction as being the product of temperature- and concentration-dependent terms,
$ r = f("temperature") dot f("concentration"). $
For elementary reactions, we can generally describe this relationship via a power-law expression.
By way of example, for the elementary reaction given by #ref(<eq:general_rxn>), $alpha ce("A ") + beta ce("B ") --> gamma ce("C ") + delta ce("D ")$, we can state
$ r prop conc("A")^alpha conc("B")^beta. $<eq:rate_propto>
Here, [A] and [B] refer to the molar concentrations of species A and B, respectively.
Each exponent is referred to as a partial order, and the overall reaction order is the sum of the exponents.
For $alpha = beta = 1$, the reaction would be said to be first-order in both #conc("A") and #conc("B"), and it would be a second-order reaction.

As a matter of convention, we will define a temperature-dependent proportionality factor, $k$, such that we can rewrite #ref(<eq:rate_propto>) as
$ r = k conc("A")^alpha conc("B")^beta. $<eq:rate_equation>
To ensure that the rate has the units given by #ref(<eq:rate_units>), the units of $k$ in #ref(<eq:rate_equation>) must be $"s "^(-1) dot ("mol"\/"m "^3)^(1 - (alpha + beta))$
when the rate is normalized on a per-unit volume basis.
Typically, $k$ is referred to as the rate constant or rate coefficient.
#footnote[While some in the community have pushed to refer to $k$ as a rate coefficient rather than a rate constant, it begs the question --- what about an equilibrium constant?]
Regardless of the name, it is important to emphasize that the value of $k$ is not a constant and can depend on several factors, most notably temperature.
The key assumption here is that the effects of temperature and effects of composition are fully decoupled.

When the reactants are gases, it is common to report the quantity of these species in terms of partial pressures, such that #ref(<eq:rate_equation>) is instead given by
$ r = k p_("A ")^alpha p_("B ")^beta, $<eq:rate_pressure>
where $p_j$ is the partial pressure of the $j$-th species (i.e. the pressure of the $j$-th species if it were to occupy the entire volume alone).
In the case of #ref(<eq:rate_pressure>), the units of $k$ instead must be given by $"s "^(-1) dot ("mol"\/"m "^3) dot "bar"^(-(alpha + beta))$.

For a set of several plausible reactions, we can fully generalize this approach by defining the rate of the $i$-th reaction as
$ r_i = k product_(j, nu_(i,j) < 0) [A_j]^(|nu_(i,j)|), $<eq:general_math_irreversible_rxn>
where $nu_(i,j)<0$ is simply stating that the multplication is being carried out for the reactants only.
As previously alluded to, if we are dealing with gas-phase species, it may be more convenient to replace $[A_j]$ with  $p_j$, but otherwise the expression remains the same.

Before continuing, it is important to reiterate that #ref(<eq:general_math_irreversible_rxn>) is only strictly valid for elementary reactions. 
If we have a non-elementary reaction, in many cases it is still possible to use 
an expression analogous to #ref(<eq:general_math_irreversible_rxn>), but there is no guarantee that the reaction orders for each species will be directly related to their stoichiometry.
In fact, the reaction orders may be non-integer or even negative, ultimately depending on the kinetics of the constituent elementary reactions.
There are also many instances, as we will show throughout this course, where a power-law expression is not suitable to describe a non-elementary reaction.

=== Reversible Reactions <reversible-reactions>

Now, we will consider a slight variation on #ref(<eq:general_rxn>) wherein the reaction is reversible:
$ alpha"A " + beta"B " eqArrow(k^(+),opposite:k^(-)) gamma"C " + delta"D ". $<eq:general_reversible_rxn>
In principle, all reactions are reversible, although if one direction is orders of magnitude slower than the other, invoking irreversibility is often a logical assumption.
We can write the rate expression for #ref(<eq:general_reversible_rxn>) in essentially the same was as for #ref(<eq:general_rxn>).
To do so, we simply write the rate expression given by #ref(<eq:rate_equation>) for each reaction --- forward and reverse:
$ r^(+) = k^(+) conc("A")^alpha conc("B")^beta $<eq:forward_reversible>
$ r^(-) = k^(-) conc("C")^gamma conc("D")^delta. $<eq:reverse_reversible>
The _net_ rate of reaction is given by the difference between #ref(<eq:forward_reversible>) and #ref(<eq:reverse_reversible>):
$ r = r^(+) - r^(-) = k^(+) conc("A")^alpha conc("B")^beta - k^(-) conc("C")^gamma conc("D")^delta. $

For a set of several plausible reversible reactions, we can fully generalize this approach by defining the net rate of reaction $i$ as
$ r_i = r_(i)^(+) - r_(i)^(-) = k_(i)^+ product_(j, nu_(i,j) < 0) [A_j]^(|nu_(i,j)|) - k_(i)^(-) product_(j, nu_(i,j)>0) [A_j]^(nu_(i,j)). $<eq:general_math_reversible_rxn>
Of course, whether one chooses to consider a reversible reaction as a single composite reaction or as two separately enumerated reactions is entirely a matter of preference and convenience.

For reversible reactions, an additional property known as the reversibility, $z_i$, can be helpful to introduce.
The reversibility of reaction $i$ is nothing more than a ratio of the forward and reverse rates:
$ z_i equiv r_(i)^(-)/r_(i)^+. $<eq:reversibility>
The reason that the reverse reaction rate is in the numerator is that a lower value of $z_i$ would imply that the forward reaction rate is greater, which is internally consistent with the notion that it would also be less reversible.



== Expressions for the Rate Constant <expressions-for-the-rate-coefficient>

=== The Arrhenius Equation <the-arrhenius-equation>

The most common expression used to evaluate the rate constant is the Arrhenius equation:
$ k = A exp(-E_"a " / (R T)), $<eq:arrhenius>
where $A$ is the pre-exponential factor, $E_"a " $ is the activation energy, $R$ is the ideal gas constant, and $T$ is the absolute temperature.
#footnote[The units of $A$ depend on the molecularity of the reaction to ensure that $k$ itself has appropriate units.]
By linearizing the equation, one finds that
#footnote[Strictly speaking, one must divide by the corresponding units before taking the logarithm, but we will omit this for brevity. Nonetheless, the units that were divded out should always be mentioned on the axis labels.]
$ ln(k) = -E_"a "/R (1/T) + ln(A), $
such that plotting $ln(k)$ vs. $1\/T$ should yield a straight line of slope $-E_"a " \/R$ and $y$-intercept of $ln(A)$, as depicted in #ref(<fig:arrhenius>).

#figure(
  image("figures/arrhenius.svg", width: 30%),
  caption: [Linearized Arrhenius plot.]
)<fig:arrhenius>


One can also use #ref(<eq:arrhenius>) to find the ratio of two rate constants, $k_1$ and $k_2$, at different absolute temperatures, $T_1$ and $T_2$, via
$ k_2 / k_1 = exp(-E_"a "/R (1/T_2 - 1/T_1)). $<eq:arrhenius_ratio>
In general, if one has a plot of $ln(k)$ vs. $1\/T$ though, it is better to rely on regression and/or interpolation rather than using #ref(<eq:arrhenius_ratio>) since the former will more naturally account for statistical uncertainty in the underlying data.

Since one is rarely studying a single, isolated, elementary reaction, there is no inherent guarantee that the Arrhenius plot will be linear when plotted as $ln(k)$ vs. $1\/T$.
#footnote[Refer to W. Wang and C.J. Roberts, "Non-Arrhenius Protein Aggregation", _AAPS J._, 15, 840--851 (2013) for several examples in the biochemistry literature.]
A common example in heterogeneous catalysis is shown in #ref(<fig:arrhenius_multiple>), where the increasing temperature can cause different kinetic processes to dominate, such as diffusion limitations, mass transfer limitations, or even the reaction proceeding homogeneously (e.g. in the gas phase).
Different linear regimes in an Arrhenius plot can also indicate a change in the underlying mechanism with temperature.

#figure(
image("figures/arrhenius_multiple.png", width: 40%),
  caption: [Different linear regimes in an Arrhenius plot for a surface-catalyzed reaction.]
)<fig:arrhenius_multiple>

The astute observer might question what thermodynamic property $E_"a " $ refers to in #ref(<eq:arrhenius>).
For now, we will simply state that the Arrhenius equation is an empirical relationship and so $E_"a "$ is best thought of as simply a parameter describing the sensitivity of the rate, and thereby rate constant, to changes in temperature.
#footnote[Speaking of empirical, the start date and temperature dependence of the Japanese cherry blossom blooming season is often modeled with the Arrhenius equation. This has been used to better understand the effects of climate change, as in P. Shi, et al., "Timing of Cherry Tree Blooming: Contrasting Effects of Rising Winter Low Temperatures and Early Spring Temperatures", _Agric. For. Meteorol._, 240, 78–89 (2017). For other unusual examples, refer to K.J. Laidler, "Unconventional Applications of the Arrhenius Law", _J. Chem. Educ._, 49, 343--344 (1972).]
We will revisit the Arrhenius equation from a more rigorous, thermodynamic perspective when we cover transition state theory.

=== Modifications to the Arrhenius Equation

While the original formulation of the Arrhenius equation remains widely used to this day, it has limitations. Most notably, #ref(<eq:arrhenius>) assumes that the pre-exponential factor is a constant.
In reality, the pre-exponential factor exhibits some degree of temperature-dependence. A modified form of the Arrhenius equation can be used to account for this fact:
$ k = A' T^n exp(-E_"a " / (R T)), $<eq:arrhenius_mod>
where $n$ is an additional fitting parameter known as the temperature exponent.
As we will show when covering transition state theory, there is theoretical justification for having $n > 0$.
The use of the modified Arrhenius equation in #ref(<eq:arrhenius_mod>) does limit the physical interpretability of the activation energy and pre-refactor terms in this way.
Regardless, the effect of $T^n$ is generally relatively small and is difficult to observe experimentally without highly precise measurements.

== Thermodynamic Equilibrium

While this is a course on the kinetics of chemical reactions, we must also acknowledge the importance of thermodynamics, which dictates the equilibrium conditions of a reacting system.

=== Equilibrium Constants Based on Concentrations and Partial Pressures<equilibrium-constants>

When a reversible reaction reaches chemical equilibrium ("eq"), the net rate is precisely zero since the forward and reverse rates of reaction must be equal to one another.
For the elementary reaction
$ alpha ce("A") + beta ce("B") eqArrow(k^+,opposite:k^(-)) gamma ce("C") + delta ce("D"), $
we have
$ 0 = k^(+) conc("A")_("eq")^alpha conc("B")_("eq")^beta - k^(-) conc("C")_("eq")^gamma conc("D")_("eq")^delta.  $<eq:reversible_eq>
We can algebraically rearrange #ref(<eq:reversible_eq>) to yield
$
k^(+) / k^(-) =
(conc("C")_("eq")^gamma conc("D")_("eq")^delta)
/
(conc("A")_("eq")^alpha conc("B")_("eq")^beta).
$<eq:reversible_eq_rearrange>
The expression given by the right-hand side of #ref(<eq:reversible_eq_rearrange>) leads to the definition of the concentration-based equilibrium constant, $K_"c "$, which can be expressed compactly as
$ K_"C " equiv product_(j) [A_j]^(nu_j) $<eq:kc>
and describes the ratio of the forward to reverse rate constants at equilibrium.
#footnote[From here on out, we will omit the "eq" subscript since it is implicit when dealing with an equilibrium constant.]
If one were to use partial pressures, $p_j$, instead of concentrations, one can define a pressure-based equilibrium constant, $K_"p "$, as
#footnote[For an ideal gas, one can conveniently state $K_"p " = K_"C " (R T)^delta$ where $delta$ is the change in stoichiometric numbers.
]
$ K_"p " equiv product_(j) p_(j)^(nu_j), $<eq:kp>
where --- for ideal gases --- we have $p_j = y_j p$ with $y_j$ the mole fraction of species $j$ and $p$ the total pressure.

=== Thermodynamic Non-Idealities

The astute observer may notice a potential conundrum with #ref(<eq:kc>) and #ref(<eq:kp>): the resulting expressions may not be dimensionless (depending on the values for $nu_(j)$).
You may recall from thermodynamics that the standard Gibbs free energy of a reaction, $Delta G^std$, can be related to the equilibrium constant via
$-R T ln(K)$, where $R$ is the ideal gas constant and $T$ is the absolute temperature.
Therefore, $K$ must be dimensionless in order to properly take the natural logarithm.
How can we rationalize this anomaly, and what kind of $K$ is appropriate to use?

The answer comes down to activities.
The activity of a species, $a_j$, is unitless and is the effective concentration (or pressure) of that species in a mixture.
For instance, the activity expression generally used to describe solids and liquids is given by
$ a_j = gamma_j [A_j]/C^std $<eq:activity1>
where $gamma_j$ is the dimensionless activity coefficient, and $C^std$ is the standard-state concentration.
The activity coefficient is simply whatever value is needed to account for non-idealities.
Physically, $gamma_j<1$ if species attract one another, whereas $gamma_j>1$ if they repel one another.
For an ideal mixture, $gamma_j$ is a value of 1.
The value for $C^std$ is typically taken as 1 mol/L for liquids but should always be mentioned when reporting data.

When describing the activities of gases, it is general convention to refer to the fugacity (the effective partial pressure) of a species, $f_j$, or a dimensionless fugacity coefficient $phi_j$ as follows:
$ a_j = f_j/p^std = phi_j y_j p/p^std, $<eq:activity2>
where $p^std$ is the standard-state reference pressure typically taken as 1 bar but should always be mentioned when reporting data.
For an ideal gas ($phi_j=1$), we can state that $f_j=y_j p =p_j$, such that the fugacity is the same as the partial pressure.
The concept of fugacity is simply a matter of terminology and bookkeeping, which will account for attractive intermolecular interactions that can occur at low temperatures or high pressures.
From a pedagogical perspective, the main takeaway is that activities are the true property of interest for equilibrium calculations, rather than concentrations of partial pressures, in order for everything to be internally consistent.

#caution[When we refer to a standard state, this is a choice that the practitioner makes. The standard state thermodynamic properties are independent of the pressure at which the reaction is actually carried out; rather, they are associated with a hypothetical process.
In contrast, the standard state does _not_ indicate a particular temperature, which must be specified separately and is typically the observed temperature. #footnote[While similar in name, the standard state is not the same concept as the standard temperature and pressure (STP).]
As such, state thermodynamic properties taken from a database may need to be adjusted to the experimentally relevant temperature.
]

=== Equilibrium Constants Based on Activity

#ref(<eq:activity1>) and #ref(<eq:activity2>) allow us to rationalize the unit conundrum of #ref(<eq:kc>) and #ref(<eq:kp>): there is a "missing" standard state reference that will ensure the equilibrium constant is unitless, even in the case of an ideal mixture.
To tie it all together, we can state
$ K_"a " = product_(j) a_(j)^(nu_j). $
As such, we have
$ Delta G^std = -R T ln(K_"a "), $
where the equilibrium constant must formally be based on activities.

For the sake of convenience later on, we can also now interrelate $K_"C "$ and $K_"a "$ as follows:
$ K_"a " =  product_j (gamma_j [A_j]/C^std)^(nu_j) = K_"C "/(C^std)^delta product_j gamma_(j)^(nu_j), $<eq:k_a_k_c_relationship>
where $delta$ is the change in stoichiometric numbers given simply as $delta equiv sum_(j) nu_j$.
If ideal conditions can be assumed, then $gamma_j=1$ and $K_"a " = K_"C "\/(C^std)^delta$.
In this form, we can see that units are appropriately addressed even though concentrations are used directly. 

In most practical cases, the deviations from non-ideality can be assumed to be small, and we will oftentimes use concentrations or partial pressures in place of activities.
However, for concentrated solutions and gases at low temperature or high pressures, the differences can become noticeable and should be considered.
#footnote[ 
To learn more about potential pitfalls when neglecting activity in equilibrium expressions, refer to C.G. McCarty, E. Vitz, "pH Paradoxes: Demonstrating That It Is Not True That pH ≡ -log[H+]", _J. Chem. Educ._, 83, 752--757 (2006).]



=== Thermodynamic Considerations

Most chemical reactions, especially in industrially relevant processes, are not carried out at thermodynamic equilibrium.
However, understanding equilibrium behavior of a chemical reaction can be incredibly important for understanding the inherit limits of operation and what reaction conditions might (or might not) be most suitable.

For this example, we will consider the ammonia synthesis reaction given by
$ ce("N2 + 3 H2 <--> 2 NH3"). $
It is known from experiments that at 298 K, $Delta G^std = -32.8 $ kJ/mol and $Delta H^std = -91.8$ kJ/mol.
The equilibrium constant for this reaction is 
$ K_"a " = (f_ce("NH3")/p^std)^2/((f_ce("N2")/p^std) (f_ce("H2")/p^std)^3) = f_ce("NH3")^2/((f_ce("N2")) (f_ce("H2"))^3) (1/p^std)^(-2). $
If we rewrite the expression in terms of mole fractions,
$ K_"a " = (phi_ce("NH3") y_ce("NH3"))^2/((phi_ce("N2") y_ce("N2")) (phi_ce("H2") y_ce("H2"))^3) (p/p^std)^(-2), $
it becomes clear that increasing the total pressure will increase the equilibrium concentration of #ce("NH3") in order for $K_"a "$ to remain constant.

We can also state that
$ K_"a " = exp(- (Delta G^std)/(R T)) = exp(- (Delta H^std)/ (R T)) exp((Delta S^std)/ R), $
such that
$ exp(- (Delta H^std)/ (R T)) exp((Delta S^std)/ R) = (phi_ce("NH3") y_ce("NH3"))^2/((phi_ce("N2") y_ce("N2")) (phi_ce("H2") y_ce("H2"))^3) (p/p^std)^(-2). $
From this expression, one can conclude that for an exothermic process (i.e. $Delta H^std <0$), the equilibrium mole fraction of #ce("NH3") will increase with decreasing temperature.

To summarize, increasing the total pressure and decreasing temperature both increase the equilibrium mole fraction of #ce("NH3").
However, this is clearly a problem when we think back to our kinetic perspective.
We know from the Arrhenius equation that the rate constant increases exponentially with temperature, so low temperatures will generally yield slower reaction rates.
This is especially relevant given that the extremely strong triple bond of #ce("N2") must be broken for ammonia to be successfully synthesized.
As a result, we have an inherent tradeoff between thermodynamics and kinetics.

The way to address this challenge is through the use of a catalyst that increases the rate of reaction, such that _relatively_ moderate temperatures can be used.
Industrially, iron-based catalysts are often used for this process, and the operating temperature is generally kept around 400 °C.
As for the pressures, they are kept relatively high at 100--200 bar to shift the equilibrium toward the production of ammonia.
The discovery of a heterogeneous catalyst that can more efficiently synthesize ammonia at near-ambient temperatures and pressures is one of the holy grails of reaction engineering. 

=== Demonstrating that $K$ is a State Function <k_state_function>

In the previous example, you may have noticed that $ce("N2 + 3 H2 <--> 2 NH3")$ is likely not an elementary reaction since it is virtually impossible for four species to react simultaneously. Here, we will demonstrate that the definition of $K_"a "$ holds true regardless of whether the reaction is elementary or non-elementary.

The most straightforward way to convince yourself of this fact is that $Delta G^std$ is a state function, which in turn must mean that $K_"a "$ is a state function as well.
If we have a non-elementary reaction with $Delta G^std$, the value for $Delta G^std$ will be the same as the sum of all the $Delta G^std$ values for the elementary steps.
Put another way, $Delta G^std$ does not depend on whether a reaction is elementary or not.
Since $K_"a " = exp(-Delta G^std\/R T)$, the same holds true for $K_"a "$.

Consider a net reaction given by 
$ ce("A") eqArrow(K) ce("D"), quad &Delta G^std $
which in reality is composed of several elementary steps:
$
ce("A") &eqArrow(K_1) ce("B"), quad &Delta G^std_1\
ce("B") &eqArrow(K_2) ce("C"), quad &Delta G^std_2\
ce("C") &eqArrow(K_3) ce("D"), quad &Delta G^std_3.
$

We know that $Delta G^std = Delta G^std_1 + Delta G^std_2 + Delta G^std_3$.
This can be expressed equivalent as
$ Delta G^std = - R T ln(K_1) - R T ln(K_2) - R T ln(K_3) $
or
$ Delta G^std =  - R T ln(K_1 K_2 K_3). $
Since $Delta G^std = - R T ln(K)$, this implies that $ K = K_1 K_2 K_3$.
Put another way,
$ K_1 K_2 K_3 = a_ce("B")/a_ce("A") a_ce("C")/a_ce("B") a_ce("D")/a_ce("C") = a_ce("D")/a_ce("A"), $
such that 
$ K = a_ce("D")/a_ce("A"). $
as expected.
Extrapolating from this analysis, it can be readily shown that the definition of $K = product_j a_(j)^(nu_j)$ holds true regardless of whether the equilibrium reaction is elementary.


=== The van~'t Hoff Equation

With our understanding of the role of thermodynamics, we can understand how Arrhenius came about his famous equation through making an analogy to prior work by van~'t Hoff.
Let us assume we have a reaction an elementary isomerization reaction of $ce("P <=> Q")$ in equilibrium.
We start with the definition of Gibbs free energy given by
$ Delta G^std = Delta H^std - T Delta S^std, $
where $Delta G^std$, $Delta H^std$, and $Delta S^std$ are the standard-state Gibbs free energy, enthalpy, and entropy changes of reaction, respectively.

As previously emphasized, the equilibrium constant-based definition of Gibbs free energy is
$ Delta G^std = -R T ln(K_"a "), $
Combining the two expressions, we have
$ ln(K_"a ") = -(Delta H^std)/ (R T) + (Delta S^std)/R. $
If we differentiate with respect to $T$ and make a fairly notable assumption that $Delta H^std$ and $Delta S^std$ are independent of temperature (an approximation that is typically reasonable when considering small differences in $T$), we arrive at
$ (dif ln(K_"a "))/(dif T) = (Delta H^std)/(R T^2), $<eq:vant_hoff>
which is the famous van~'t Hoff equation.

If we rewrite #ref(<eq:vant_hoff>) using $K_"a " = k^+\/k^-$,
we can state
$ (dif ln(k^(+) \/ k^(-))) / (dif T) = (Delta H^std) / (R T^2) $
and thereby
$ (dif ln(k^(+))) / (dif T) - (dif ln(k^(-))) / (dif T) = (Delta H^std) / (R T^2). $
By invoking $Delta H^std = E_"a,f" - E_"a,r"$,
where $E_"a,f"$ and $E_"a,r"$ are activation energies for the forward and reverse reactions, respectively, Arrhenius concluded that, largely by analogy, the following is likely to be true:
$
(dif ln(k^(+))) / (dif T) = E_"a,f" / (R T^2),quad
(dif ln(k^(-))) / (dif T) = E_"a,r" / (R T^2)
$
which is equivalent to #ref(<eq:arrhenius>) following integration.
Clearly, this "derivation" lacks rigor, and we will later show that $Delta H^std$ is not generally synonymous with the difference in activation energy.
For now, we will accept the applicability of the Arrhenius equation largely based on empirical evidence until we cover transition state theory.

== Differential Kinetic Analysis

Consider a reaction of the form #ce("A + B -> C").
In practice, we may not know _a priori_ whether this reaction proceeds precisely as written (i.e. if it is elementary or not).
We may propose a rate law of the form
$ r = k conc("A")^alpha conc("B")^beta, $
although there is no guarantee that $alpha$ and $beta$ are the stoichiometric numbers.
#footnote[
Additionally, there is no guarantee that the rate law model parameters will be a sufficient fit to the experimental data over a range of operating conditions.
]

This is where the concept of "apparent" kinetics comes in.
Strictly speaking, $k$, $alpha$, and $beta$ that are determined from experiments will be "apparent" rate parameters, meaning that they are based on empirical observations that may or may not reflect the underlying (i.e. intrinsic) reactions.

=== Apparent Activation Energy

The intrinsic activation energy differs from the apparent activation energy in that the latter may represent the kinetics of many constituent reactions, as depicted in #ref(<fig:apparent_activation>).
#figure(
image("figures/apparent_activation.png", width: 50%),
  caption: [Potential energy diagram highlighting how the apparent activation energy, $E_"app"$, accounts for the kinetics of multiple underlying processes that collectively make up a non-elementary reaction.]
)<fig:apparent_activation>

The linearized form of the Arrhenius equation is so widely used that the definition of the apparent activation energy is generally derived from this functional form by taking the partial derivative with respect to temperature:
$ ln(k_"app") &= - E_"app"/R (1/T) + ln(A_"app") $
$ (diff ln(k_"app"))/(diff T) &= -E_"app"/R (diff (1/T))/(diff T) $<eq:apparent_e_a_pre>
$ (diff ln(k_"app"))/(diff T) &=  E_"app"/(R T^2) $
$ E_"app" &equiv R T^2 (diff ln(k_"app"))/(diff T). $<eq:apparent_e_a>

Here, the "app" subscript is referring to an apparent (i.e. observed) rate constant determined from experiments, meaning that it may describe a net, non-elementary reaction consisting of several elementary steps.

While #ref(<eq:apparent_e_a>) is the formal definition of the apparent activation energy,
#footnote[Some references define the apparent activation energy as $E_"app" equiv R T^2 (diff ln(r))/(diff T)$. However, this is only strictly true if one is not varying the reactant concentrations. Similarly, one should exercise caution when interpreting rate law parameters from a plot of $ln(r)$ vs. $1\/T$ if the reactant concentrations may vary.]
we should not lose sight of the fact that we can still use a plot of $ln(k_"app")$ vs. $1\/T$ to back out $E_"app"$ over a particular range of temperatures.
In fact, that is precisely what #ref(<eq:apparent_e_a_pre>) is stating: take the instantaneous slope at a particular value of $T$ in a plot of $ln(k_"app")$ vs. $1\/T$, and you will get a value of $-E_"app"\/R$.

=== Apparent Reaction Orders

We can also define an apparent reaction order, $alpha_(j,"app")$, for each species in a similar manner.
First, for a reaction #ce("A + B -> C"), we will postulate a power-law rate expression of the form
$ r = k_"app" conc("A")^(alpha) conc("B")^beta, $
which can be rewritten as
$ ln(r) = ln(k_"app") + alpha ln(conc("A")) + beta ln(conc("B")). $

We can determine $alpha$ by holding $ln(conc("B"))$ fixed and finding the slope in a plot of $ln(r)$ vs. $ln(conc("A"))$.
#footnote[This assumes that $beta$ does not change with #conc("A"), which is a fairly reasonable assumption unless changes in #conc("A") alter the mechanism.]
Similarly, we can determine $beta$ by holding $ln(conc("A"))$ fixed and finding the slope in a plot of $ln(r)$ vs. $ln(conc("B"))$.
In differential form, this can be expressed as follows:
$ alpha_(j,"app") equiv  [A_j] ((diff ln(r))/(diff [A_j]))_([A_i], i!=j). $<eq:apparent_order>
Given enough rate and concentration data, one can also carry out a multiple linear regression analysis to determine the apparent reaction orders if preferred.

It is worth noting, as we will demonstrate throughout this course, that apparent reaction orders of non-elementary reactions may be non-integer or even negative.
Reaction orders will appear as zero if the concentration of a particular species does not notably influence the observed rate, as is commonly the case if that species is in great excess.
#footnote[For additional discussion about apparent 0#super[th] order kinetics, refer to F.J. Arnáiz, "Mice in the Box for Zero-Order Kinetics", _J. Chem. Educ._, 76, 10, 1458 (1999).]

=== Practical Measurements

A few questions may naturally arise when thinking about how one would measure the rate parameters in practice.
Perhaps the most notable to ask is: how can one monitor the change in rate as a function of only _one_ reactant's concentration?
Arguably the most natural solution is to carry out the reaction such that all reactants except one are present in large excess, such that the reaction appears 0#super[th] order in every species except the one for which the reation order is desired.

Beyond relying on reagents being in exccess, one can carry out an _initial rate_ experiment, where you measure $r$ at extremely small values of $t$.
Consider $conc("A")_0$ and $conc("B")_0$ as the initial concentration of reactants.
For $t approx 0$, the concentrations of the reactants remain largely unchanged from their initial values: $conc("A") approx conc("A")_0$ and $conc("B") approx conc("B")_0$.
This allows you to measure changes in $r$ with respect to isolated changes in either $conc("A")$ or $conc("B")$ by modifying the initial concentration of either species.

A very similar concept exists for surface-catalyzed reaction, in which the reaction is typically run in a differential reactor.
A differential reactor has an extremely small amount of catalyst, such that the _conversion_ of the starting reagents is extremely small.
The conversion of a species, $X_j$, is defined as
$ X_j equiv ("moles of " A_j "reacted")/("moles of " A_j "fed"). $
For a reaction taking place in a constant-volume reactor like a flask, 
$ X_j = ([A_j]_0 - [A_j])/[A_j]_0. $
For a differential reactor where reagents are flowed through a bed of catalyst, it is oftentime more natural to use molar flow rates in place of concentrations.
The differential reactor is analogous to an initial rate experiment, as it  allows the practitioner to measure $r$ in a regime where the consumption of starting reagents is near-infinitesimal.

== Integrated Rate Expressions <integrated-rate-expressions>

When studying a new reaction of interest, one is typically interested in how the species concentrations change as a function of time.
In this subsection, we will derive analytical expressions to describe this phenomenon.
Throughout this subsection, we will make an important assumption:
$ r_j equiv 1/V ((dif n_j) / (dif t)) = (dif [A_j]) / (dif t). $
This implies that we have kinetic data from a reaction taking place with a constant volume batch reactor or that we are focusing on a specific volume element within the reacting vessel.

=== Irreversible, First-Order Reaction <irreversible-first-order-reaction>

For simplicity, we will start by considering an irreversible, first-order, elementary reaction given by the expression
$ ce("A") fwdArrow(k) ce("B"). $
The rate law for this reaction can be given by
$ r_ce("A ") = (dif conc("A")) / (dif t) = -k conc("A"). $
Separating the variables and integrating this expression yields
#footnote[The \' marks are included simply to indicate a variable of integration that differs from the integral bounds.]
$ integral_(conc("A")_0)^(conc("A")) 1 / conc("A")' dif conc("A")' = -k integral_(0)^(t) dif t' $
$ ln(conc("A") / conc("A")_0)  = -k t. $<eq:first_order_irreversible>
We can simplify #ref(<eq:first_order_irreversible>) to
$ conc("A") = conc("A")_0 e^(-k t). $<eq:first_order_irreversible2>
From #ref(<eq:first_order_irreversible2>), a plot of $ln(conc("A"))$ vs. $t$ from the experimental data should be linear for a first-order, irreversible reaction.
#footnote[If experimental data is collected and found to fit well to #ref(<eq:first_order_irreversible2>), it would be consistent with a first-order, irreversible reaction but that does not guarantee that it is truly first order nor does it imply that the reaction is necessarily elementary. For instance, a true rate law of $r = k conc("A") conc("B")$ might appear first order in #conc("A") if #conc("B") is in great excesss, such that only changes in $conc("A")$ appreciably alter the rate. This would be more precisely referred to as pseudo-first order in #conc("A").]
As a sanity check, we can see that when $t->infinity$, $[A] -> 0$ as expected.

=== Irreversible Reactions of Arbitrary Order <irreversible-reactions-of-arbitrary-order>

For simplicity, we will start by considering an irreversible, elementary reaction given by the expression
$ n ce("A") fwdArrow(k) m ce("B") $
where $n$ is an arbitrary stoichiometric number.
The rate of change in $conc("A")$ can be given by
$ r_ce("A ") = (dif conc("A")) / (dif t) = -n k conc("A")^n. $
#caution[Most sources write this as $r_"A " = -k conc("A")^n$ and continue the derivation as such. However, if we are specifically considering an elementary reaction where we have the convention that $r=r_j\/nu_j$, then including the stoichiometric coefficient as a multiplicative factor is important for internal consistency. To convince yourself of this, you already know that the rate law is $r = k conc("A")^n$ for this elementary reaction. Therefore, $r_"A "$ must be $-n k conc("A")^n$ in order for $r = -r_"A "\/n$.]

Separating the variables and integrating this expression yields
$ integral_(conc("A")_0)^conc("A") 1 / conc("A")'^n dif conc("A")' = -n k integral_0^t dif t' $
$ conc("A")^(-n+1) / (-n+1) - conc("A")_0^(-n+1) / (-n+1) = -n k t quad (n != 1). $<eq:irreversible_n_order>
We can simplify #ref(<eq:irreversible_n_order>) to
$ conc("A")^(-n+1) = conc("A")_0^(-n+1) - n (-n+1) k t $
$ conc("A")^(-n+1) = conc("A")_0^(-n+1) + n(n-1) k t $<eq:irreversible_n_order_final>
From #ref(<eq:irreversible_n_order_final>), a plot of $conc("A")^(-n+1)$ vs. $t$ from the experimental data should be linear for a given stoichiometric coefficient $n$.
The same procedure can be done for bimolecular reactions; the only difference is that the algebra is slightly more complicated.

It is always a good practice to do some sanity checks.
For this analysis, let us take $n=2$ just for demonstration purposes and see what happens when $t->infinity$.
$ 1/conc("A") = 1/conc("A")_0 + 2 k dot infinity $
$ conc("A") = 1/(1/conc("A")_0 + infinity) = 0. $
This is just as we would expect.

Now let us take $n=-2$ instead for demonstration purposes and see what happens when $t->infinity$.
This time, we have
$ conc("A")^3 = conc("A")_(0)^3 + 6 k dot infinity $
$ conc("A") = (conc("A")_(0)^3 + 6 k dot infinity)^(1\/3) $
At first glance, this may seem unusual.
We have #conc("A") increasing with $t$ without bound despite being the reactant.
The reason for this seemingly odd behavior is that we specifically derived the integrated rate law for an _elementary_ reaction.
If we have $n<0$, then we must instead be describing the reverse reaction, such that $conc("A")$ increases with time from its starting value of $conc("A")_0$.

=== Coupled Reactions <coupled-reactions>

Naturally, many reactions involve multiple mechanistic steps.
For instance, consider the following consecutive reaction, which we will assume to consist of two distinct elementary steps:
$
ce("A") fwdArrow(k_1) ce("B") fwdArrow(k_2) ce("C").
$
As usual, we will write out the rates of change for each species:
$
r_"A " &= (dif conc("A")) / (dif t) = -k_1 conc("A")\
r_"B " &= (dif conc("B")) / (dif t) = k_1 conc("A") - k_2 conc("B")\
r_"C " &= (dif conc("C")) / (dif t) = k_2 conc("B").
$

To understand the behavior of this coupled reaction, our next step is to integrate with respect to time.
We will consider the reaction starting at $t=0$ with only the reagent (A) and no intermediates (B) or products (C).
As before, we know that
$ conc("A") = conc("A")_0 e^(-k_1 t). $<eq:sequentiial_A>
Now we can substitute #ref(<eq:sequentiial_A>) into the expression for $r_"B "$ to arrive at
$ (dif conc("B")) / (dif t) = k_1 conc("A")_0 e^(-k_1 t) - k_2 conc("B"). $
The integration here is a bit messy, but one can show with some effort that
$ conc("B") = conc("A")_0 (k_1) / (k_2 - k_1) (e^(-k_1 t) - e^(-k_2 t)). $<eq:sequential_B>
From here, we could substitute #ref(<eq:sequential_B>) into the expression for $r_ce("C")$ and integrate to find an expression for $conc("C")$ with respect to time.
That said, it is a fair bit simpler to just invoke the conservation of mass:
$ conc("C") = conc("A")_0 - conc("A") -conc("B"), $
where we have assumed $conc("B")_0 = conc("C")_0 = 0$.

#plot[#align(center)[https://marimo.app/l/2emhpu]]

== Stochastic Reactions <stochastic-reactions>

_This is an "advanced topic" not discussed in class and provided solely for the interested reader._

In the previous subsection, we took a deterministic approach to modeling the behavior of reaction events.
In the limit of small numbers of molecules, however, this approach begins to break down.
In this subsection, we will take a stochastic, atomistic approach to understanding chemical reactions.
A stochastic approach is particularly common in biochemical simulations, such as reactions taking place within cellular environments.

For the sake of example, consider the serial reactions $ce("A->B->C")$, which can be written as two discrete steps:
$ 
ce("A") &fwdArrow(k_1) ce("B")\
ce("B") &fwdArrow(k_2) ce("C").
$
Here, we are considering each reaction as a discrete event involving individual molecules.
We define the propensity, $a_j$, of each reaction in an analogous way as we define the reaction rate:
$ a_1 = k_1 x_"A ", quad a_2 = k_2 x_"B ", $
where each $x_j$ is the discrete number of $j$ molecules in the reactor (rather than the continuous value of concentration).
We will also consider the time $t$ to be discretized, such that we are simulating reaction snapshots (or lack thereof) at each time step.

To carry out this stochastic simulation, we follow the algorithm of Gillepsie.
#footnote[For further details, refer to D.T. Gillepsie, "Stochastic Simulation of Chemical Kinetics", _Annu. Rev. Phys. Chem._, 58 (2007).]
We will describe this algorithm by way of example, assuming here that $k_1=2$ and $k_2= 1$.
With this, the procedure looks like the following:

1. Initialize the time to $t=t_0$ ($t_0=0$ is the natural choice) and the number of molecules (also known as the state vector) to $bold(x) = bold(x)_0$. 
  - For instance, if we start with 100 molecules of A, 0 molecules of B, and 0 molecules of C, we would have $bold(x) = [100, 0, 0]$.
2. Calculate each propensity $a_i$.
  - With the above example, that would be $a_1 = 2 dot 100 = 200$ and $a_2 = 1 dot 0 = 0$.
3. Calculate the sum of the propensities, $a_"tot" equiv sum_i a_i$.
  - Here, that would be $a_"tot" = 200+0=200.$
4. Calculate the time step, $tau$, given by $tau equiv -ln(R_1)\/a_"tot"$, where $R_1$ is a (pseudo) random number between 0 and 1.
  - Let's say my random number is 0.73 (this is admittedly not very random). We would have $tau = -ln(0.73)\/100 approx 0.00157$.
5. Update the time based on the value of $tau$.
  - For this example, that would be $t_1=t_0 + tau = 0.00157$.
6. Determine which reaction is likely to occur at this new time point by finding the smallest value of $j$ that satisfies $sum_(i=1)^j a_i > R_2 dot a_"tot"$ where $R_2$ is another (pseudo) random number.
  - If we say that $R_2=0.31$, then we would ask: is $a_1 > R_2 dot a_"tot"$? The answer is yes for our example because $200 > 0.31 dot 200$, so we take $j=1$ and state that reaction 1 will occur at our new time point $t_1$. #footnote[You might notice that it will always be the case that $j=1$ is selected by this algorithm in the first time step. This is to be expected because #ce("B->C") cannot occur unless #ce("A->B") happens first. When we update the simulation, this will no longer be the case, and the probability that reaction $j=2$ will occur will increase as the population of #ce("B") increases.] 
7. We update the number of molecules to be $bold(x)_1 = bold(x)_0 + Delta bold(x)_j$.
  - For our example, that would be $bold(x)_1 = [100, 0, 0] + [-1, 1, 0] = [99, 1, 0].$ 
8. Finally, we repeat steps 2--8 until $t$ becomes sufficiently large that the reaction landscape has been fully characterized.

Since the end result is a single stochastic simulation, one might rerun the entire simulation to identify if the behavior changes.
In the limit of small numbers of molecules, it is very possible for the stochastic simulation to converge to results that differ from the deterministic solution.
In this scenario, it is critical to use the stochastic approach.
In the limit of large numbers of molecules, one gets the same solution as typical power-law kinetics, which is a nice way to justify its validity.

#plot[#align(center)[https://marimo.app/l/blubdx]]

= Analytical Rate Expressions for Reaction Mechanisms <analytical-rate-expressions>

== Simplifying Rate Expressions

In #ref(<integrated-rate-expressions>), we demonstrated how to derive rate expressions via integration of differential equations.
In practice, it is often difficult to arrive at a closed-form, analytical solution for reaction mechanisms of practical interest.
Instead, one must either numerically integrate a system of differential equations or make simplifying approximations to derive an algebraic solution.
In this section, we will focus on common ways to derive analytical rate expressions that can provide significant insight into a proposed reaction mechanism.

Throughout this section, we will also relax the assumption that we know in advance what the mechanism may be.
In general, the procedure to determine a plausible mechanism is as follows:

+ Postulate elementary steps, specifically bond-making and bond-breaking events.
+ If possible, derive a closed-form solution to the rate law that describes the rate as a function of experimental observables. This may involve making several assumptions based on what is known about the reaction itself.
+ Test the rate law against any available data and repeat the process as-needed.

While this procedure may show that a proposed reaction mechanism is consistent with the experimental data, it is worth keeping in mind that this does not definitively prove that the individual elementary steps are complete and accurate.

=== The Pseudo-Steady State Hypothesis

==== Mathematical Argument

One of the most widely used assumptions in reaction network analysis is the pseudo-state state hypothesis (PSSH), which allows for the assumption that the net rate of change of a species, $r_j$, can be approximated as zero if $A_j$ is extremely short-lived.
This approximation is most commonly invoked for high-energy intermediates, such as radical species, and is valid after a (typically brief) induction period.
It generally requires that the intrinsic rate of consumption of the intermediate is much greater than the rate(s) of production, such as species B in the elementary reaction sequence
$ ce("A") fwdArrow(k_1) ce("B") fwdArrow(k_2) ce("C"), quad k_2>>k_1. $

If we were to invoke PSSH, we would state $r_ce("B") approx 0$.
Note that it does _not_ mean that we directly set #conc("B") to zero or a constant value, which would have the potential to over-simplify the equations, as we will justify below.

To demonstrate the PSSH, we will revisit the solutions to the differential equations that were derived for the #ce("A->B->C") series reaction in #ref(<coupled-reactions>):
$ conc("A") = conc("A")_0 e^(-k_1 t). $
$ conc("B") = conc("A")_0 (k_1 ) / (k_2 - k_1) (e^(-k_1 t) - e^(-k_2 t)). $
$ conc("C") = conc("A")_0 - conc("A") -conc("B"). $

If we invoke $k_2>>k_1$, we can approximate the above expressions as follows:
#footnote[Note that, although $k_1\/k_2 <<0$, we cannot directly set $conc("B") = 0$ because never producing B would mean that C would never be produced.]
$ conc("A") = conc("A")_0 e^(-k_1 t) $
$ conc("B") = conc("A")_0 (k_1) / (k_2) e^(-k_1 t) = k_1/k_2 conc("A") $<eq:b_before_pssh>
$ conc("C") = conc("A")_0 (1- e^(-k_1 t)). $

Now what would the expressions look like if we specifically invoked the PSSH condition of $r_ce("B") approx 0$ from the very beginning?
Given that we are dealing with elementary reactions, we can state
$ r_ce("B") = k_1 conc("A") - k_2 conc("B") $
and, therefore, by setting $r_ce("B") approx 0$ we have
$ conc("B") = k_1/k_2 conc("A"). $
This is exactly the same expression as #ref(<eq:b_before_pssh>), justifying our use of the PSSH in this scenario.
Also note that #conc("B") is not a constant, and --- while certainly small --- it changes linearly with #conc("A").


If we take the time derivatives of each expression, we can observe some other interesting behavior:
$ r_"A " = (dif conc("A"))/(dif t) = - conc("A")_0 k_1 e^(-k_1 t) $
$ r_"B " = (dif conc("B"))/(dif t) = - conc("A")_0 (k_1^2) / (k_2) e^(-k_1 t) (approx 0) $
$ r_"C " = (dif conc("C"))/(dif t) = conc("A")_0 k_1 e^(-k_1 t). $

We can see from the above expressions that $r_"A " = - r_"C "$, which is another feature of PSSH when dealing with series reactions and is to be expected since $r_"B "$ is negligible in comparison (i.e. A can be thought of as almost instantaneously being transformed into C given the short lifetime of B).
Of course, this is merely an _approximation_, but it is a quite useful one.

#plot[#align(center)[https://marimo.app/l/bxr9r8]]

==== Demonstration

We will now demonstrate the utility of the PSSH in action.
Consider the following reaction:
$ ce("2NO + O2 -> 2NO2") $<eq:no2_rxn>
One might propose based on the stoichiometry an elementary rate law of the form
$ r =^? k conc("NO")^2 conc("O2"). $<eq:no2_rate_law>
It is known from experiments that the reaction does appear to be second order in #conc("NO") and first order in #ce("O2").
However, termolecular reactions are extremely rare, so one might conclude that it would be unlikely for the reaction to proceed as written despite the observed rate law. 
Unusually, $r$ has also been observed to _decrease_ with increasing temperature.
These details allow us to definitively conclude that #ref(<eq:no2_rxn>) cannot actually be an elementary reaction. 
We will rationalize this so-called "anti-Arrhenius" behavior below.

First, we must propose a mechanism.
Since it is not expected that you know the intricacies of atmospheric chemistry, we will simply take the following as provided to us:
$ ce("NO + O2") eqArrow(k_1, opposite: k_(-1)) ce("NO3^∙") $
$ ce("NO3^∙ + NO") fwdArrow(k_2) ce("2 NO2") $<eq:no2_slow_step>
Then we write out the elementary rate law for $r_ce("NO2")$ based on #ref(<eq:no2_slow_step>):
$ r_ce("NO2") = 2k_2 [ce("NO3^∙")] [ce("NO")]. $<eq:rate_no2>
Note the factor of 2 in #ref(<eq:rate_no2>), which is needed because two #ce("NO2") molecules are produced for every reaction of #ce("NO3^∙") and #ce("NO"), as originally noted in #ref(<eq:stoichs>).

Since #ce("NO3^∙") is a short-lived radical species, we can choose to invoke the PSSH to simplify matters.
Namely, the PSSH lets us state that the #ce("NO3^∙") will have a net rate of formation that is essentially zero:
$ r_ce("NO3^∙") approx 0 = k_1 conc("NO") conc("O2") - k_(-1) conc("NO3^∙") - k_2 conc("NO3^∙") conc("NO"). $<eq:rate_no3_rad>
Ideally, we would like to substitute in for #ce("NO3^∙") in #ref(<eq:rate_no2>) since it is not easy to observe experimentally given its short lifetime.
With this in mind, we will solve for #ce("NO3^∙") in #ref(<eq:rate_no3_rad>) to get
$ conc("NO3^∙") = (k_1 conc("NO") conc("O2")) / (k_(-1) + k_2 conc("NO")). $<eq:rate_no3_rad_rearrange>
Substituting in #ref(<eq:rate_no3_rad_rearrange>) into #ref(<eq:rate_no2>) yields
$ r_ce("NO2") = (2 k_1 k_2 conc("NO")^2 conc("O2")) / (k_(-1) + k_2 conc("NO")). $<eq:rate_no2_rearrange>
From here, the rate of reaction, $r$, can be computed simply as $r_ce("NO2")\/2$ since $r = r_j\/nu_j$.

In principle, we can stop with the above expression.
However, it would not explain the observed rate law.
If we invoke that $k_(-1)>>k_2 conc("NO")$,
#footnote[This would be the case if the reverse reaction rate for Step 1 is much faster than Step 2 since this would imply that $k_(-1) conc("NO3^∙")>>k_2 conc("NO3^∙") conc("NO")$ and, therefore, $k_(-1) >>k_2 conc("NO")$.]
the rate expression would simplify to
$ r_ce("NO2") = (2 k_1 k_2) / (k_(-1)) conc("NO")^2 conc("O2"). $<eq:r_no2_k>

This leads us to the first important observation.
While the simplified rate law above is second order in #conc("NO") and first order in #conc("O2"), which matches the stoichiometry of the net reaction #ce("2NO + O2 -> 2 NO2"), the rate law is clearly not that of an elementary reaction.
This example illustrates that even if the apparent orders of a reaction match those from the elementary rate law, it does not necessarily imply that a reaction is elementary.

The second important observation is that #ref(<eq:r_no2_k>) can be rewritten as
$ r_ce("NO2") = (2 A_1 A_2) / (A_(-1)) exp(-(E_"a,1" - E_("a,"-1) + E_"a,2") / (R T)) conc("NO")^2 conc("O2"). $<eq:no2_rate_law_real>
If we define
$ A_"app" equiv (A_1 A_2) / A_(-1) $
$ E_"a,app" equiv E_"a,1" - E_("a,"-1) + E_"a,2", $
where $A_"app"$ and $E_"a,app"$ are an apparent pre-factor and activation barrier,
#footnote[Note that applying the definition of the apparent activation energy (#ref(<eq:apparent_e_a>)) to $k_"app" equiv k_1 k_2 \/ k_(-1)$ would yield the same expression for $E_"a,app"$.]
then
$ r_ce("NO2") = 2 A_"app" exp(-E_"a,app" / (R T)) conc("NO")^2 conc("O2"). $
Depending on the relative magnitudes of the intrinsic activation energies, it is possible for $E_"a,app" < 0$, in which case the reaction would have anti-Arrhenius behavior where the rate actually _decreases_ with increasing temperature.
Indeed, this reaction is known from experiments to have an empirically measured kinetic barrier of --3.3 kJ/mol (albeit via a slightly more complex mechanism than the elementary steps proposed here).

=== Radical Chain Reactions <radical-chain-propagation>

Consider the following gas-phase, photochemical reaction:
$ ce("H2 + Br2 -> 2 HBr"). $<eq:hbr_eq>
If the reaction were elementary, the rate law would be $r = k conc("H2") conc("Br2")$.
However, this functional form is known to disagree with kinetic data, which points to a one-half order in #ce("Br2") at low conversions of the reagents.
For this reason, one can immediately conclude that the reaction is not elementary and must, instead, be composed of several substituent elementary reactions with the formation of transient --- but kinetically important --- intermediates.
A more representative rate law for #ref(<eq:hbr_eq>) can be found by postulating elementary steps, applying approximations as needed, and solving for a closed-form solution, which we will do below.

First, we must come up with plausible mechanistic steps.
For pedagogical purposes, we will start by assuming that the following reaction mechanism is known and will back-justify why it makes sense:
$ ce("Br2") &eqArrow(k_1,opposite:k_(-1)) ce("2 Br^∙") $<eq:rxn_br_1>
$ ce("Br^∙ + H2") &eqArrow(k_2,opposite:k_(-2)) ce("HBr + H^∙") $<eq:rxn_br_2>
$ ce("H^∙ + Br2") &fwdArrow(k_3) ce("HBr + Br^∙"). $<eq:rxn_br_3>
The reaction scheme is depicted in 
#figure(
image("figures/br_chain_propagation.svg", width: 33%),
  caption: [Reaction cycle for the radical chain reaction involving #ce("Br2") and #ce("H2").]
)<fig:br_chain_prop>



This mechanism is known as a radical chain propagation mechanism, as radicals are perpetually consumed and produced over the course of the reaction.
In the forward direction, #ref(<eq:rxn_br_1>) is known as an initiation reaction since it generates radical species that will initiate the larger reaction cascade.
In the reverse direction, #ref(<eq:rxn_br_1>) is known as a termination reaction since it decreases the active pool of radicals.
#ref(<eq:rxn_br_2>) and #ref(<eq:rxn_br_3>) are known as propagation reactions, as they produce at least as many radicals as are consumed and, thereby, they propagate the chain reaction.
In this case, the propagation reactions are also the source of the observed product, #ce("HBr").
Note how each elementary step in the mechanism consists of clearly defined collisions between molecules and at most two reacting species (ternary reactions are rare, albeit not impossible, in general).

With a greater understanding of the proposed mechanism, we will start by writing the elementary rate law for the production of #ce("HBr"), the species produced in the net reaction:
$ r_ce("HBr") = k_2 conc("Br^∙") conc("H2") - k_(-2) conc("HBr") conc("H^∙") + k_3 conc("H^∙") conc("Br2"). $<eq:rate_hbr>
To simplify #ref(<eq:rate_hbr>), we will invoke the PSSH, approximating the rate of formation/consumption of both the #ce("Br^∙") and #ce("H^∙") radicals as zero given their transient nature:
$
r_ce("Br^∙") &approx 0 = 2 k_1 conc("Br2") - 2k_(-1) conc("Br^∙")^2 - k_2 conc("Br^∙") conc("H2") + k_(-2) conc("HBr") conc("H^∙") + k_3 conc("H^∙") conc("Br2")
$<eq:rate_br_rad>
$
r_ce("H^∙") &approx 0 = k_2 conc("Br^∙") conc("H2") - k_(-2) conc("HBr") conc("H^∙") - k_3 conc("H^∙") conc("Br2").
$<eq:rate_h_rad>
Ultimately, we want to be able to write $r_ce("HBr")$ without any transient intermediates in it.
We can start by noting that if we add together #ref(<eq:rate_br_rad>) and #ref(<eq:rate_h_rad>), then we get
#footnote[Naturally, we ignore the negative root, which would be unphysical.]
$ conc("Br^∙") = sqrt((k_1 conc("Br2")) / k_(-1)). $<eq:rate_br_dot>

#alternate[
Before continuing, it is worth noting a slightly simpler way we could have approached this problem.
In invoking the pseudo-steady state hypothesis on $ce("Br^∙")$, we were able to assume that $r_ce("Br^∙") approx 0$.
Since initiation and termination reactions alter the concentration of radical species but propagation reactions collectively do not, we can state that the rate of initiation must equal the rate of termination for the free radical species.
Applying this to $ce("Br^∙")$ results in
$ 2 k_1 conc("Br2") = 2k_(-1) conc("Br^∙")^2, $
which is the same as
$ conc("Br^∙") = sqrt((k_1 conc("Br2"))/k_(-1)) $
that was found with #ref(<eq:rate_br_dot>).
]

With an expression for #conc("Br^∙"), we can plug #ref(<eq:rate_br_dot>) plug back into #ref(<eq:rate_h_rad>) to yield
$ conc("H^∙") = (k_2 sqrt((k_1 conc("Br2")) / k_(-1)) conc("H2")) / (k_(-2) conc("HBr") + k_3 conc("Br2")). $<eq:rate_h_dot>
Plugging #ref(<eq:rate_br_dot>) and #ref(<eq:rate_h_dot>) into #ref(<eq:rate_hbr>) yields the desired rate expression, after slogging through some algebra:
$ r_ce("HBr") = 2k_2 sqrt(k_1 / k_(-1)) conc("H2") sqrt(conc("Br2")) (1 + (k_(-2) conc("HBr")) / (k_3 conc("Br2")))^(-1). $<eq:rate_hbr_big>

As was originally stated, it is known from experiments that the rate expression is one-half order in #conc("Br2") at low conversions.
A low conversion implies that there is negligible product, such that $conc("HBr") approx 0$. With this observation, we can simplify #ref(<eq:rate_hbr_big>) to
$ r_ce("HBr") = 2k_2 sqrt(k_1 / k_(-1)) conc("H2") sqrt(conc("Br2")). $
With such expressions, one will typically combine rate constants for the sake of simplicity and to reduce the number of fitted parameters when validating a rate expression.
Here, we can define
$ k_"app" equiv k_2 sqrt(k_1 / k_(-1)), $
such that
$ r_ce("HBr") = 2 k_"app" conc("H2") sqrt(conc("Br2")) $<eq:rate_hbr_low_conversions>
at low conversions.
As expected, #ref(<eq:rate_hbr_low_conversions>) has an apparent one-half order in #conc("Br2") in this regime and is consistent with observations from experiments.
Of course, that does not necessarily prove that the proposed mechanism is the true mechanism, but it does mean it is at the very least plausible.

=== Using Bond-Dissociation Enthalpies

When looking at the proposed mechanism in #ref(<radical-chain-propagation>), one might ask why certain plausible reactions were omitted.
For instance, it would be worth asking why we did not include the following reactions:
$ ce("H2 -> 2H^∙"), quad quad ce("HBr -> H^∙ + Br^∙"). $
This can be explained from an analysis of the bond dissociation energies for each relevant species.
A bond dissociation energy is, as the name suggests, the reaction energy required to dissociate a chemical bond.
The room-temperature bond dissociation enthalpies, $Delta H^std_"rxn"$, for several gas-phase species relevant to the #ce("H2 + Br -> 2 HBr") radical chain reaction can be found in #ref(<table:bdes>).

#figure(
  table(
    columns: 2,
    table.header(
      [Species],
      [$Delta H^std_"rxn"$ (kJ/mol)]
    ),
    [#ce("H2")], [436],
    [#ce("HBr")], [366],
    [#ce("Br2")], [194]
  ),
  caption: "Bond dissociation enthalpies at 298 K."
)  <table:bdes>

As is evident from looking at a potential energy diagram (#ref(<fig:pe_diagram>)), the definition of the transition state implies that it must be _at least_ as large as the bond-dissociation energy, so bond-dissociation energies can be thought of as a lower-bound on the possible value for $E_"a "$.
With this in mind, based on #ref(<table:bdes>) we can reasonably conclude that the rate constants for #ce("H2 -> 2H^∙") and #ce("HBr -> H^∙ + Br^∙") would likely be small relative to the rate constants of the other steps, such that their rates of reaction can be neglected under most reasonable reaction conditions.
It is for this reason that these steps are excluded in the provided mechanism.


#figure(
  image("figures/pe_diagram.svg", width: 40%),
  caption: [Sample potential energy diagram for an endothermic reaction.]
)<fig:pe_diagram>


Later in the course, we will describe a related rule-of-thumb known as the Bell--Evans--Polanyi (BEP) principle, which states that $E_"a " prop Delta H^std$ for a given reaction family.#footnote[It is a bit of a tautology in that a reaction family is one that follows the Bell--Evans--Polanyi principle.]
Namely, as we will later show,
$ E_"a "  = E_0 + alpha Delta H^std, $
where this relationship can be thought of as being largely empirical, and $0<= alpha <=1$.
Nonetheless, it implies that --- for a given reaction family --- the more endothermic the reaction enthalpy is, the higher the activation barrier tends to be for that process.
The $Delta H^std$ values can be obtained from experiments or tabulated thermochemical data,
#footnote[Recall from thermodynamics that $Delta H^std$, which is the enthalpy change associated with the reaction, can be calculated from the tabulated standard state enthalpy of formation, $Delta_"f " H_(j)^std$, for each species via $Delta H^std = sum_j nu_j Delta_"f " H_(j)^std$.]
such as the CRC Handbook of Chemistry and Physics.

=== Quasi-Equilibrium Approximation

==== Description

Distinct from the PSSH, we can consider a scenario where one or more of the reversible reaction steps are effectively in equilibrium, which we will refer to as the quasi-equilibrium hypothesis.
Note that we are referring to a _reaction_ here rather than the lifetime of a _species_, the latter of which was the case when invoking PSSH.
For the sake of clarity, the PSSH and quasi-equilibrium approximations generally differ in which steps are considered relatively fast.
For example,
$ eqArrow("slow",opposite:"fast") ce("X") fwdArrow("fast") quad ("PSSH"), quad quad quad eqArrow("fast",opposite:"fast") ce("X") fwdArrow("slow") quad ("quasi-equilibrium"), $
where, for the model reaction schemes, we have $k_(1)<<k_(-1),k_2$ and $k_(1),k_(-1)>>k_2$ for PSSH and quasi-equilibrium, respectively.
#footnote[For additional details, refers to J.F. Perez-Benito, "Some Considerations on the Fundamentals of Chemical Kinetics: Steady State, Quasi-Equilibrium, and Transition State Theory", _J. Chem. Educ._, 94, 1238--1246 (2017).]
PSSH would imply that $r_ce("X") approx 0$, and the quasi-equilibrium approxmation would imply that $r_1 = r_(1)^+ - r_(1)^- approx 0$, where $r_1$ is the net rate of the fast equilibrium step.
The way to rationalize the quasi-equilibrium approximation is that perturbing the system slightly (e.g. removing #ce("X")) would cause a near-immediate return to its original state (e.g. by producing more #ce("X")).
In other words, the reaction is rapidly equilibrated.

==== Demonstration

Let us consider the net reaction #ce("2 NO + Br2 <--> 2 NOBr"). A proposed mechanism can be given as follows:
$
ce("NO + Br2") &eqArrow(k_1,opposite:k_(-1)) ce("NOBr_2") quad ("fast")\
ce("NOBr2 + NO") &fwdArrow(k_2) ce("2 NOBr") quad ("slow"),
$
where we are stating $k_(1), k_(-1) >> k_2$, and we have an intermediate #ce("NOBr2") that does not appear in the net reaction equation.

We can write out our rate of product production, $r_ce("P")$, as
$ r_"P " = 2 k_2 conc("NOBr2") conc("NO"). $<eq:rate_pre_eq>
Since we would not like to deal with a concentration of an intermediate in our rate expression, we will need to get rid of #conc("NOBr2").
If were to proceed as usual, we would write out $r_ce("NOBr2")$ and continue from there.
However, by acknowledging the fact that the first step (both forward and reverse) is much faster than the second step, we can assume that it is in quasi-equilibrium.
This allows us to state that the forward and reverse rates for Step 1 are approximately the same:
$ k_1 conc("NO") conc("Br2") approx k_(-1) conc("NOBr2") $
or
$ conc("NOBr2") approx K_1 conc("NO") conc("Br2"), $
as expected from the definition of the equilibrium constant: $K_1 equiv k_1 \/k_(-1)$.
Substituting in #conc("NOBr2") into #ref(<eq:rate_pre_eq>) yields
$ r_"P " = 2 k_2  K_1 conc("NO")^2 conc("Br2"). $
Naturally, we can combine constants together via $k_"app" equiv k_2 K_1$ if desired and arrive at
$ r_"P " = 2 k_"app" conc("NO")^2 conc("Br2"). $
The rate law for the reaction, $r$, would simply be $r = r_ce("P")\/2$ due to the fact that $r = r_j\/nu_j$.
The rate law derived here is consistent with the experimentally observed rate law, which is reassuring.
Before continuing, it must be re-emphasized that the result would be quite different if we instead invoked PSSH where we would instead be saying that $r_ce("NOBr2")=0$.

=== Rate-Determining Step <rate-determining-step>

Here, we will take our assumptions to the logical extreme and consider what happens when one reaction step completely dominates the overall kinetics: a concept known as the rate-determining step.

Consider the following reaction:

$ ce("NO2 + CO -> NO + CO2"). $

From experiments, it is known that the rate appears to only be dependent on #conc("NO2"), for which it is second order in #conc("NO2").
Since this does not match the stoichiometry, we cacn immediately conclude that there must be elementary steps not shown.
In fact, there are two:
$
ce("2 NO2") &fwdArrow(k_1) ce("NO + NO3^∙") quad ("slow")\
ce("NO3^∙ + CO") &eqArrow(k_2,opposite:k_(-2)) ce("NO2 + CO2") quad ("fast"),
$
where we are stating $r_1 << r_(-2),r_2$.

For the reaction to be second-order in #conc("NO2") and not depend on other species, it is evident that the first step must be substantially slower than the second step such that it dominates the kinetic expression.
Indeed, that is the case: the bimolecular reaction of two #ce("NO2") molecules is substantially slower than the reaction of #ce("NO3^∙") and #ce("CO").
The rate law in this case can be approximated as
$ r = k_1 conc("NO2")^2, $<eq:rate_no>
which is based solely on the rate-determing step (i.e. the first reaction).
It does not contain any transient intermediates in it, so we can leave it as-is without further manipulation.
Clearly, #ref(<eq:rate_no>) agrees with the experimental observation that the rate appears to be dependent on $conc("NO2")^2$ but not on #conc("CO").

This example also highlights how the presence of a rate-determining step inherently implies that any other reversible steps are inherently in quasi-equilibrium.
This is because quasi-equilibrium can generally be invoked when the forward and reverse rates are much faster than the surrounding steps in the proposed mechanism.

#caution[
While it is tempting to invoke a rate-determining step since it greatly simplifies the algebra, one should be cautious about doing so without being extremely precise about what is being invoked.
Let's revisit a similar set of reactions:
$
ce("A") &eqArrow(k_1, opposite:k_(-1)) ce("B") quad (k_1 "extremely small")\
ce("B") &fwdArrow(k_2) ce("C"),
$
where $k_1<<k_(-1),k_2$.
Here, we are considering a set of reactions where the forward rate constant of the first step is very small with respect to that of the reverse reaction and second step.
Without invoking any assumptions about rate-determining steps yet, the rate of reaction can be derived in the usual way as
$ r = (k_1 k_2) / (k_(-1) + k_2) conc("A"). $<eq:rds_1>
If we were to invoke the rate-determining step concept, we might naively state
$ r = k_1 conc("A"). $<eq:rds_2>
However, it is clear that this is not generally valid.
If $k_(-1) << k_2$, then indeed we can state that #ref(<eq:rds_1>) can be correctly approximated by #ref(<eq:rds_2>).
However, we have made no specific assumption in the problem statement about the value of $k_(-1)$ with respect to $k_2$.
If $k_(-1) >> k_2$, then we have
$ r = (k_1 k_2) / k_(-1) conc("A"), $
which does not equal the rate-determining step solution.
Furthermore, it would be difficult to justify calling $ce("A") --> ce("B")$ the rate-determining step if $k_1$ has precisely the same weight as $k_(-1)$ and $k_2$.
This exercise is simply to demonstrate that a rate-determining step cannot necessarily be invoked from information about $k$ for a particular step, particularly when reversible reactions are considered.
#footnote[For additional details, refer to S. Kozuch, J.M.L. Martin, "The Rate-Determining Step is Dead. Long Live the Rate-Determining State!", _ChemPhysChem_, 12, 1413--1418 (2011).]
]


=== Pressure-Dependent Rate Constants <pressure-dependent-rate-coefficients>

While pressure is not a component in the definition of the intrinsic rate constant, it turns out that some reactions do exhibit pressure-dependent apparent rate constants, which we will demonstrate below.

Consider the following isomerization reaction:
$ ce("CH3NC -> CH3CN"). $
If this reaction were elementary, first-order kinetics would be observed. However, experiments have shown that this is only true at high pressures.
Herein, we will explain this anomaly by invoking the Lindemann mechanism.
#footnote[The Lindemann mechanism is useful from a pedagogical standpoint, but, in reality, it is not quantitatively accurate. For a more accurate treatment, refer to the Rice--Ramsperger--Kassel--Marcus (RRKM) theory.]

The Lindemann mechanism states that a unimolecular reaction #ce("A -> B") can be described by the following steps:
$ ce("A + M") eqArrow(k_1,opposite:k_(-1)) ce("A^* + M") $<eq:lindemann_rxn>
$ ce("A^*") fwdArrow(k_2) ce("B "), $
where #ce("A ") is the reactant, #ce("A^*") is the "activated" form of #ce("A "), and #ce("M ") is a gas molecule that imparts sufficient energy into #ce("A ") to initiate the reaction.

The rate of production of B based on the above mechanism can be given by
$ r_ce("B ") = k_2 conc("A^*"). $
Since #ce("A^*") is a high-energy species that can spontaneously decompose, we can safely invoke the PSSH like we have previously done for radical species.
This allows us to state
$ r_ce("A^*") approx 0 = k_1 conc("A") conc("M") - k_(-1) conc("A^*") conc("M") - k_2 conc("A^*"). $
Solving for #conc("A^*") yields
$ conc("A^*") = (k_1 conc("A") conc("M") ) / (k_(-1) conc("M") + k_2). $<eq:lindemann_a_rad>
Combining #ref(<eq:lindemann_rxn>) and #ref(<eq:lindemann_a_rad>) results in the following rate expression without any transient intermediates in it:
$ r_ce("B ") = (k_1 k_2 conc("M") conc("A")) / (k_(-1) conc("M") + k_2). $

Let's now consider the behavior in different pressure regimes.
In the low pressure limit, $conc("M")$ is small since there are few other particles for #ce("A ") to collide with.
In such scenarios, it is reasonable to propose that we have $k_(-1) conc("M") << k_2$, such that
$ r_("B ") approx k_1 conc("A") conc("M") quad ("low" P). $
In the high pressure limit, $conc("M") >> 0$ since there are many opportunities for collisions, resulting in $k_(-1) conc("M") >> k_2$, such that
$ r_("B ") approx (k_1 k_2) / (k_(-1)) conc("A") quad ("high" P). $
From this analysis, we can conclude that at low pressures, the rate expression would appear as second-order overall, whereas at high pressures, the rate expression would appear first-order overall.


== Enzyme Kinetics <enzyme-kinetics>

=== Michaelis--Mentin Kinetics <michaelis-menten-kinetics>

The reactions described in this chapter are entirely general.
In fact, the same governing principles can be used to understand biological reactions like those occurring in the presence of enzymes.
Enzymes, like all catalysts, alter the reaction mechanism in such a way as to reduce the apparent activation energy, increasing the overall rate of reaction.
In biological systems, this is achieved through the adsorption of a substrate (i.e. reactant), S, at a binding pocket of the enzyme to ultimately produce a given product P via the presence of one or more catalytic intermediates ES (e.g. an enzyme--substrate complex). If we define the free enzyme as E, we can write a simplified reaction mechanism as follows:
$
ce("E + S") &eqArrow(k_1,opposite:k_(-1)) ce("ES") \
ce("ES") &fwdArrow(k_2) ce("E + P").
$

The goal is to find an expression for the rate of production of P. From the elementary reaction steps, we can state that
#footnote[In the literature, you may see $v$ used in place of $r_"P "$. The $v$ is meant to symbolize reaction velocity.]
$ r_"P " = k_2 conc("ES"). $<eq:rate_michaelis_menten_p>
However, the concentration of ES is not a quantity than can be easily measured. As such, our next step is to find a way to rewrite the concentration of ES in terms of observables and constants.

Writing out the rate expression for ES, we get
$ r_"ES" approx 0 = k_1 conc("E") conc("S") - k_(-1) conc("ES") - k_2 conc("ES"), $<eq:michaelis_menten_es>
where we have set the expression equal to zero by invoking the pseudo-state state hypothesis given the transient nature of the ES intermediate.
#footnote[In reality, there is an induction period before pseudo-steady state is reached. If the enzyme concentration is much larger than that of the substrate, this induction period may be large. However, it is typically the case that there is much more substrate than enzyme.]
Solving for the concentration of ES in #ref(<eq:michaelis_menten_es>) yields
$ conc("ES") = (k_1 conc("E") conc("S")) / (k_(-1) + k_2). $<eq:mm_es>
Knowing the concentration of free enzyme, #conc("E"), at a given point in time can be quite difficult.
Fortunately, we can take advantage of the conservation of mass to simplify things a bit. 
Namely, we can state that the total concentration of enzyme, $#conc("E")_0$, does not change over the course of the reaction such that
$ conc("E")_0 = conc("E") + conc("ES"), $<eq:michaelis_menten_e_t>
where we have implicitly assumed that we are starting the reaction from scratch, such that $conc("ES")_0 = 0$.
By solving for the #conc("E") in #ref(<eq:michaelis_menten_e_t>) and substituting the resulting expression into #ref(<eq:mm_es>), we arrive at
$ conc("ES") = (k_1 (conc("E")_0 - conc("ES")) conc("S")) / (k_(-1) + k_2) $
$ conc("ES") + (k_1 conc("ES") conc("S")) / (k_(-1) + k_2) = (k_1 conc("E")_0 conc("S")) / (k_(-1) + k_2) $
$ conc("ES")  = ((k_1 conc("E")_0 conc("S")) / (k_(-1) + k_2))/(1 + (k_1 conc("S")) / (k_(-1) + k_2)) $
$ conc("ES") = (conc("E")_0 conc("S")) / ((k_(-1) + k_2) / k_1 + conc("S")). $<eq:michaelis_menten_es2>
Substituting #ref(<eq:michaelis_menten_es2>) into #ref(<eq:rate_michaelis_menten_p>) yields
$ r_"P " = (k_2 conc("E")_0 conc("S")) / ( (k_(-1) + k_2) / k_1 + conc("S")). $<eq:michaelis_menten_p2>
We have now arrived at a closed-form, analytical solution for the rate of generation of the product that is solely dependent on experimental observables and constants.
In practice, #ref(<eq:michaelis_menten_p2>) is typically rewritten as
$ r_"P " = (V_"max" conc("S")) / (K_"M " + conc("S")), $<eq:michaelis_menten>
where
$ V_"max" equiv k_2 conc("E")_0 $
and
$ K_"M " equiv (k_(-1) + k_2) / k_1. $
The expression given in #ref(<eq:michaelis_menten>) is known as the Michaelis--Menten equation and is a simple model of enzyme reaction kinetics.

In the limit of $conc("S")>>K_"M "$, the rate is pseudo zeroth-order in #conc("S"), approaching a value of $V_"max"$, thereby giving it its name of the maximum rate.
In the limit of $conc("S")<<K_"M "$, the rate is pseudo first-order in #conc("S").
$K_"M "$ is known as the Michaelis constant and describes the concentration of S at which the reaction rate is half of $V_"max"$.

It is worth noting that S is the free substrate, whereas one often measures the total substrate in experiments.
In practice, one can typically assume that the concentration of substrate is much greater than the total enzyme concentration, such that the total substrate can be used.
This is known as the free ligand approximation.
We have also assumed in this derivation that the production of P is irreversible. One can re-derive $r_"P "$ with reversible product binding in precisely the same way; the math simply gets slightly messier.

=== Linearizing the Michelis--Menten Equation <linearizing-the-michaelis-menten-equation>

When analyzing experimental data, it is oftentimes useful to fit to a linear equation.
This can be achieved quite readily be rearranging the Michaelis--Menten equation to
$ 1/r_"P " = K_"M "/V_"max" 1/conc("S") + 1/V_"max". $

Here, the inverse of the reaction rate is plotted against the inverse concentration of the substrate, which in turn can be used to obtain $K_"M "$ and  $V_"max"$ from the slope and $y$-intercept.
Such a plot is known as a Linweaver--Burk plot.

#plot[#align(center)[https://marimo.app/l/pbceww]]

There is, however, a very important limitation in using Lineweaver--Burk plots.
By relying on inverse rate on the $y$-axis and inverse concentration on the $x$-axis, the experimental data will not be distributed evenly across the range of values.
This may result in poorly determined fit parameters $V_"max"$ and $K_"M "$.
There are many other ways to linearize the Michaelis--Menten equation, which have their own limitations.
In modern times, it is generally recommended to do nonlinear regression on the Michaelis--Menten equation directly to avoid such complications.

#plot[#align(center)[https://marimo.app/l/f43w0q]]

= Rate Expressions for Heterogeneous Reactions <rate-expressions-for-heterogeneous-reactions>

In #ref(<analytical-rate-expressions>), we were implicitly focused on homogeneous reactions.
That said, many reactions are carried out in the presence of a heterogeneous catalyst that accelerates the overall rate of reaction.
In this section, we will shift our focus to surface catalysis, wherein a reactant in a continuum diffuses into a catalyst particle, adsorbs to the surface, reacts to form a new species, desorbs, diffuses out of the catalyst particle, and re-enters the continuum.
We will focus on developing a basic kinetic model from elementary steps including adsorption, desorption, and surface reaction.

== Adsorption Rate Laws <heterogeneous-catalysis-adsorption-mechanisms>

=== Physisorption and Chemisorption <types-of-adsorption>

Adsorption of the substrate to the catalyst surface is the critical first step in a catalytic reaction.
Before proceeding, we should clarify some terminology.
Adsorption is the process by which a gas or solution phase molecule or atom binds to a surface.
The adsorbate is the molecule or atom binding to the surface, and the adsorbent is the surface itself.
Note that absorption is different than adsorption; absorption is the bulk uptake of a fluid without a corresponding phase change.

Molecule A can adsorb to a surface site \* via one of two main mechanisms: chemisorption or physisorption.
Chemisorption involves the formation of a bond between the adsorbate and the surface and involves a change in the electronic structure of the adsorbate.
In contrast, physisorption is largely due to electrostatic and van der Waals interactions without a significant change in the electronic structure of the adsorbate.
Chemisorption is typically a much stronger interaction than physisorption but both are incredibly important.
Additionally, it is possible for a molecule to dissociate upon adsorption to form two separate adsorbate.
This does not need to be a diatomic molecule.
For instance, #ce("CH4") can dissociatively adsorb to form #ce("CH3^∙") and #ce("H^∙").
There is typically a notable activation barrier for the dissociative adsorption process.

The adsorption process can happen on a variety of different types of surfaces.
For instance, one might consider the adsorption of a molecule to a metal surface like Pd or Rh.
Alternatively, one might consider the adsorption of a molecule along the internal surface of a highly porous material like an aluminosilicate zeolite.
Porous materials are particularly unique choices for adsorbents, as they can have incredibly high surface areas; for instance, nanoporous solids known as metal--organic frameworks can have surface areas up to 7000 $"m "^2$/g, such that one gram of material can have the surface area of a football field.

=== Molecular Adsorption <molecular-adsorption>

Since adsorption is a pre -requisite for heterogeneous reactions, we will focus on developing a kinetic description of the adsorption process.
We will start with the simple case of one adsorbate that adsorbs in a non-dissociative (i.e. molecular) fashion to a surface site:
$ ce("A + *") eqArrow(k_"ads",opposite:k_"des") ce("A^*"). $<eq:molecular_ads>
For example, this could be #ce("CO + * <=> CO^*").
The equation for the rate of adsorption and desorption can be written as
$
r_"ads" &= k_"ads" p_"A " conc("*")\
r_"des" &= k_"des" conc("A^*").
$<eq:rate_ads_des>
From here, we are going to investigate equilibrium adsorption behavior to better understand this phenomena.
In other words, we will consider the situation where the rates of the adsorption and desorption processes are equal (i.e. $r_"ads" = r_"des"$), such that
$
K_"ads" equiv k_"ads" / k_"des" &= conc("A^*") / (p_"A " conc("*")).
$<eq:molecular_ads_eq_constant>

It is difficult to directly interpret #ref(<eq:molecular_ads_eq_constant>) given the fact that #conc("*") is difficult to measure.
Instead, the property that is generally measured is $#conc("*")_0$, which is the number density of all adsorption sites both vacant and occupied.
We can write a site balance that states the number of sites does not change:
$ conc("*")_0 equiv conc("*") + sum_j [A_j^"*"], $
where the summation accounts for all possible adsorbate species at the surface sites.
#footnote[This assumes that the concentration of surface sites remains constant over the course of the reaction, which is not always the case (e.g. if catalyst deactivation or coking occurs, such that the sites must be regenerated).]
In the case of #ref(<eq:molecular_ads>), the site balance becomes
$ conc("*")_0 = conc("*") + conc("A^*"). $<eq:molecular_ads_site_balance>
Solving for #conc("*") in #ref(<eq:molecular_ads_eq_constant>) and substituting into #ref(<eq:molecular_ads_site_balance>) yields
$
conc("*")_0 &= conc("A^*") / (K_"ads" p_"A ") + conc("A^*")\
conc("*")_0 &= conc("A^*") (1/(K_"ads" p_"A ") + 1).
$
Typically, a parameter $theta_j$ is defined that describes the fractional surface coverage of species $j$, such that
$ theta_"A " equiv conc("A^*") / conc("*")_0 = 1 / (1 / (K_"ads" p_ce("A")) + 1) = (K_"ads" p_"A ") / (1 + K_"ads" p_"A "). $<eq:langmuir>

#ref(<eq:langmuir>) is referred to as a Langmuir adsorption isotherm, which describes the equilibrium adsorption behavior and is depicted in #ref(<fig:langmuir>).
For $p_ce("A")K_"ads"<<1$ (i.e. at low partial pressure of #ce("A")), $theta_ce("A")-> K_"ads" p_ce("A")$.
For $p_ce("A")K_"ads">>1$ (i.e. at high partial pressure of #ce("A")), $theta_ce("A")->1$.  

#figure(
  image("figures/langmuir.svg", width: 33%),
  caption: [Langmuir adsorption isotherm.]
)<fig:langmuir>

It is important to note several implicit assumptions for classical Langmuir theory to hold.
Generally speaking, a Langmuir adsorption isotherm does not account for multilayer coverages.
As such, it is best suited for describing monolayer adsorption. 
Furthermore, it assumes that all the adsorption sites are energetically uniform, such that the probability of an adsorbate binding to a given adsorption site is identical across the catalyst surface.
#footnote[Metal ions or clusters hosted on an amorphous support like silica is a clear example of a material where the active sites are energetically diverse.]
It also assumes that there are no adsorbate--adsorbate interactions, which in reality can depend on the adsorbate and the interatomic distance between adsorption sites.
All of these are fairly substantial approximations that can break down in real catalytic systems.
Nonetheless, the Langmuir adsorption isotherm remains a useful qualitative tool for understanding the kinetics of adsorption.

It is useful to think about how $theta_"A "$ might change with temperature and how it is related to thermodynamics.
For this analysis, we note that $K_"ads"$ is a function of temperature and that we can write
$ theta_"A " = (exp(- (Delta H^std - T Delta S^std) / (R T)) p_"A ") / (1 + exp(- (Delta H^std - T Delta S^std) / (R T)) p_"A "). $
From this expression, it can be readily shown that increasing $T$ will decrease $theta_"A "$.
This is easily explained in physical terms by the fact that the adsorbate has more kinetic energy and, therefore, is more likely to be liberated from the surface.
It can also be shown that decreasing $Delta H^std$ (i.e. making it more negative since adsorption is generally exothermic) will increase $theta_"A "$.
Again, this can be easily explained by the fact that a more exothermic adsorption energy implies a stronger bond to the surface, making the species less likely to desorb.

#plot[#align(center)[https://marimo.app/l/e0dix3.]]

Before continuing, it should be noted that we can rewrite our rates of adsorption and desorption in terms of fractional coverage as follows:
$ r_"ads" &= k_"ads" p_"A " conc("*") = k_"ads" p_"A " conc("*")_0 theta_ce("*") = k_"ads" p_"A " conc("*")_0 (1-theta_ce("A")) \
r_"des" &= k_"des" conc("A^*") = k_"des" conc("*")_0 theta_ce("A"),
$
where we took advantage of the fact that $sum_j theta_j =1$, where $j$ includes both the adsorbate species and the vacant sites.
We can only use #ref(<eq:langmuir>) for the definition of $theta_ce("A")$, however, if the adsorption or desorption process is equilibrated.

=== Multi-Site Adsorption

It is natural to think about how one might modify the Langmuir equation for a scenario where there are two or more energetically distinct adsorption sites on the catalyst surface.
The procedure is no different than before if we make a leap of faith and assume that the adsorption at each binding site is independent from one another, such that we can treat the adsorption as a sum of individual Langmuir models.
In other words, we can write
$ theta_"A " = sum_s M_s (K_("ads",s) p_("A ")) / (1 + K_("ads",s) p_("A ")), $
where $K_("ads",s)$ is now an "equilibrium constant" for the adsorption of species A at site $s$, and $M_s$ is the maximum capacity of site $s$.
Here, $M_s$ and $K_("ads",s)$ are both fitting parameters determined from an experimentally measured adsorption isotherm. 
Generally, it is impossible to enumerate all possible surface sites, and even if one could, the number of fitting parameters would be huge.
Instead, this model is typically used when it is clear there are (for instance) two major yet distinct adsorption sites that adsorbates can bind to.

// #self[
//   Showcase #ce("O2")/#ce("N2") example in https://doi.org/10.1021/jacs.9b12401.
// ]

=== Competitive Adsorption <competitive-adsorption>

Continuing our journey with the Langmuir adsorption model, we will now consider the scenario where we have multiple species competing for surface sites, such as

$
ce("A + * <=> A^*")\
ce("B + * <=> B^*")
$

For the sake of simplicity, we will invoke the same assumptions for the Langmuir adsorption isotherm as in #ref(<molecular-adsorption>) with the additional caveat that each site must only hold one molecule of A or B but not both simultaneously.

Under equilibrium conditions, we can equate the rates of adsorption and desorption:
$
k_"ads,A" p_"A " conc("*") &= k_"des" conc("A^*")\
k_"ads,B" p_"B " conc("*") &= k_"des" conc("B^*"),
$
such that
$ K_"ads,A " = conc("A^*") / (p_("A ") conc("*")) $<eq:competitive_ads_Ka>
$ K_"ads,B" = conc("B^*") / (p_("B ") conc("*")). $<eq:competitive_ads_Kb>
The site balance can be rewritten as
$ conc("*")_0 = conc("*") + conc("A^*") + conc("B^*"). $<eq:competitive_ads_site_balance>
Once again, we seek to get rid of #conc("*"). To do so, we start by solving for #conc("*") in #ref(<eq:competitive_ads_Ka>) and plugging this into #ref(<eq:competitive_ads_site_balance>):
$ conc("*")_0 = conc("A^*") / (K_"ads,A" p_("A ")) + conc("A^*") + conc("B^*"). $<eq:competitive_ads_site_balance2>
Since we will ultimately want an expression for $theta_"A "$ that is independent of #conc("B^*"), we will solve for #conc("B^*") in #ref(<eq:competitive_ads_Kb>) and plug this into #ref(<eq:competitive_ads_site_balance2>):
$ conc("*")_0 = conc("A^*") / (K_"ads,A" p_("A ")) + conc("A^*") + K_"ads,B" p_("B ") conc("*"). $
It looks like we have #conc("*") in our expression again, so we substitute in #conc("*") from #ref(<eq:competitive_ads_Ka>) to get
$
conc("*")_0& =
conc("A^*") / (K_"ads,A" p_("A "))
+ conc("A^*")
+ (conc("A^*") K_"ads,B" p_("B ")) / (K_"ads,A" p_("A "))
 \
conc("*")_0 &=
conc("A^*") (1/(K_"ads,A" p_("A "))
+ 1 + (K_"ads,B" p_("B "))/(K_"ads,A" p_("A "))).
$
Therefore,
$
theta_"A " equiv conc("A^*")/conc("*")_0 =
1 /
(
  1 / (K_"ads,A" p_("A "))
  + 1
  + (K_"ads,B" p_("B ")) / (K_"ads,A" p_("A "))
)
=
(K_"ads,A" p_("A ")) / (1 + K_"ads,A" p_("A ") + K_"ads,B" p_("B ")).
$

By analogy for species B, the following result can be found:
$ theta_"B " = (K_"ads,B" p_("B ")) / (1 + K_"ads,A" p_("A ") + K_"ads,B" p_("B ")). $

As you might be able to already tell, we can generalize the adsorption isotherm for arbitrary numbers of adsorbates as
$ theta_"A " = (K_("ads,A ") p_("A "))/(1+sum_j K_("ads",j) p_j). $<eq:general_multi_langmuir>

Finally, we can revisit our rates of adsorption and desorption to write them in terms of fractional coverages:
$
r_("ads",ce("A")) &= k_("ads",ce("A")) p_"A " conc("*") = k_("ads",ce("A")) p_"A " conc("*")_0 theta_* = k_("ads",ce("A")) p_"A " conc("*")_0 (1-theta_ce("A") - theta_ce("B"))  \
r_("des",ce("A")) &= k_("des",ce("A")) conc("A^*") = k_("des",ce("A")) conc("*")_0 theta_ce("A").
$
If the adsorption and desorption processes are equilibrated, then we can use #ref(<eq:general_multi_langmuir>) to substitute in for $theta_ce("A")$ and $theta_ce("B")$.

=== Dissociative Adsorption <dissociative-adsorption>

Now, we will consider a dissociative adsorption process:
#footnote[Dissociative chemisorption is both an adsorption and surface reaction process. We include it in the adsorption section to compare and contrast expressions for the adsorption isotherm.]
$ ce("A2 + 2*") eqArrow(k_"ads",opposite:k_"des") ce("2 A^*"). $<eq:rxn_a2>
We will again invoke the typical assumptions of the Langmuir adsorption isotherm with the additional caveat that the individual #ce("A^*") species adsorb onto separate surface sites.

However, there is an important subtlety that needs to be emphasized.
For dissociative adsorption to occur, there must be a pair of adjacent sites for the adsorbate to dissociate onto.
Put another way, it is not just _any_ two surface sites.
To make sure we do not make a logical error here, we will temporarily rewrite our equation as
$ ce("A2 + **") eqArrow(k_"ads",opposite:k_"des") ce("A**A"), $
where #ce("**") indicates a pair of adjacent, accessible surface sites, and #ce("A**A") is a pair of adjacent, occupied surface sites.

We now need an expression for #conc("**") and #conc("A**A").
We will start by introducing the answer and then justifying why it is the case.
Namely, lattice statistics allows us to state:
$ conc("**") = z/2 conc("*")^2/conc("*")_0 $
and
$ conc("A**A") = z/2 (conc("A^*")^2)/conc("*")_0, $
where $z$ is the coordination number of the site, and a $1\/2$ factor is introduced to avoid over-counting when dealing with identical pairs of species or sites.
#footnote[Implicit in the lattice statistics-based expression is the so-called Hinshelwood assumption, wherein the adsorbates are considered to be randomly distributed on the surface without any degree of spatial correlation.]

#figure(
  image("figures/lattice.svg", width: 25%),
  caption: [Schematic of adsorption on a square lattice with vacant sites and adsorbed A species. This cartoon example of a surface would have $z=4$ for each site.]
)

The way we can justify the above expressions is as follows.
Consider the expression for #conc("**").
We want to find the number density of adjacent pairs of vacant sites.
The probability of randomly picking a vacant site on the lattice is $conc("*")\/conc("*")_0$, and we can start by considering all possible sites on the surface: $conc("*")_0 dot.op (conc("*")\/conc("*")_0)$.
Once we have picked a vacant site, we want to see if we can pick another one that is adjacent to our choice.
For this, the probability of finding a vacant site is again $conc("*")\/conc("*")_0$, but this time we are not considering all possible sites ($conc("*")_0$); rather, we are considering only the sites adjacent to the first pick (i.e. the number of coordinating sites), such that we have $z dot.op (conc("*")\/conc("*")_0)$.
For the likelihood of both events to occurring, we multiply the two independent event likelihoods together to arrive at
$conc("**") =z dot.op (conc("*")^2\/conc("*")_0)$.
The final factor of $1\/2$ comes in to prevent double-counting when dealing with statistically identical pairs of sites or species on the surface.

The equations for the rate of adsorption and desorption can now be written as $
r_"ads" &= k_"ads" p_ce("A2") conc("**") = (z k_"ads" p_ce("A2") conc("*")^2)/(2 conc("*")_0)\
r_"des" &= k_"des" conc("A**A") = (z k_"des" conc("A^*")^2)/(2 conc("*")_0) .
$
In other words, there is an additional factor of $z\/2 conc("*")_0$ that needs to be included than if one were to write the elementary rate law solely based on #ref(<eq:rxn_a2>).
Setting both expressions equal to one another to invoke equilibrium conditions yields
$ K_"ads" = conc("A^*")^2 / (p_ce("A2") conc("*")^2). $<eq:dissociative_K_a>
#caution[If we had not accounted for the statistical siting, we would instead have $r_"ads"=k_"ads" p_ce("A2") conc("*")^2$ and $r_"des"=k_"des" conc("A^*")^2$, which will overestimate the rates of adsorption and desorption and change the units on our rate constant. That said, there would be no change in our expression for $K_"ads"$.]
The site balance is given by
$ conc("*")_0 = conc("*") + conc("A^*"). $<eq:dissociative_site_balance>
Since we want to have an expression for $theta_ce("A")$ that is independent of #conc("*"), we can solve for #conc("*") in #ref(<eq:dissociative_K_a>) and plug it into #ref(<eq:dissociative_site_balance>) to get
$
conc("*")_0 &= conc("A^*") / sqrt(K_"ads" p_ce("A2")) + conc("A^*")\
conc("*")_0 &= conc("A^*") (1 / sqrt(K_"ads" p_ce("A2")) + 1).
$

Therefore,
$
theta_"A " &= 1 / (1 / sqrt(K_"ads" p_ce("A2")) + 1)
$
$
theta_"A " &= sqrt(K_"ads" p_("A ")) / (1 + sqrt(K_"ads" p_("A "))).
$<eq:competitive_theta>
We can see that when $sqrt(K_"ads" p_ce("A")) <<1$ (i.e. in the limit of low partial pressures of #ce("A2")), $theta_ce("A")->sqrt(K_"ads" p_ce("A"))$, which is significantly different than the linear behavior observed for the non-dissociative Langmuir adsorption isotherm.

We can also revisit our rate expressions to write them in terms of surface coverages of observable species:
$
r_"ads" &= (z k_"ads" p_ce("A2") conc("*")^2)/(2 conc("*")_0) = z/2 k_"ads" p_ce("A2") conc("*")_0 theta_*^2 = z/2 k_"ads" p_ce("A2") conc("*")_0 (1-theta_ce("A"))^2\
r_"des" &= (z k_"des" conc("A^*")^2)/(2 conc("*")_0) = z/2 k_"des" conc("*")_0 theta_ce("A")^2 .
$
If the adsorption and desorption processes are equilibrated, we can use #ref(<eq:competitive_theta>) to determine the value of $theta_ce("A")$.

=== Non-Langmuir Isotherms

==== Henry's Law

The simplest adsorption model is given by Henry's isotherm.
Henry's isotherm states, in analogy with Henry's law, that
$ theta_"A " = K_"H " p_"A " $
where $K_"H "$ is Henry's adsorption constant and is generally a fitting parameter.
Henry's isotherm states that there is a direct, linear relationship between coverage and partial pressure.
This is clearly false for many adsorption processes but is valid when the partial pressure of the adsorbate is low, such that there are negligible interactions between adsorbed molecules and the adsorption sites are plentiful.
We can see that Henry's isotherm is related to the Langmuir isotherm in the limit of small $p_"A "$:
$ theta_"A " = (K_"ads" p_"A ") / (1 + K_"ads" p_"A ") -> K_"ads" p_"A " quad (K_"ads" p_"A "<<1). $
That said, it is also true that Henry's isotherm can be applicable at more appreciable partial pressures, provided the adsorption of the molecule is very weak.
This is by no means an improvement on the Langmuir model but can make sense to use if the adsorption isotherm is in the linear regime.

==== Two-Parameter, Empirical Isotherm Models

We can also consider models that capture behavior the Langmuir model does not.
As previously discussed, the Langmuir model of adsorption made some critical approximations.
Perhaps the most notable is the assumption that the adsorption enthalpy of each site is constant (i.e. independent of coverage).
There are many models that attempt to directly or indirectly capture this behavior, some of which are outlined below:
#footnote[For some cautionary comments about the Temkin isotherm, refer to K. Chu, "Revisiting the Temkin Isotherm: Dimensional Inconsistency and Approximate Forms", _Ind. Eng. Chem. Res._, 60, 13140--13147 (2021).]
$
theta_"A " &= alpha p_"A "^(1\/beta) quad ("Freundlich isotherm")
$
$
theta_"A " &= (K_"ads" p_"A ") / (1 + (K_"ads" p_"A ")^beta)^(1\/beta) quad ("Tóth isotherm")
$
$
theta_"A " &= (R T)/alpha ln(beta p_"A ") quad ("Temkin isotherm").
$
There are many other functional forms that have been proposed and that can potentially be dreamed up.
In all three cases presented here, there is additional flexibility compared to the (single-site) Langmuir adsorption isotherm, as there are two fitting parameters instead of one if the practitioner is attempting to fit an experimentally measured adsorption isotherm.
Naturally, each of these models have their own limitations.
Most notably, many alternate models have the unphysical scenario where $theta_"A "$ can become greater than one for large $p_"A "$.
Nonetheless, these models can capture adsorption on surface complexities better than the single-site Langmuir equation because of their additional flexibility, at the expense of reduced interpretability.
Both the Freundlich and Tóth isotherms were proposed as ways to deal with surface heterogeneity.
The Temkin isotherm was proposed as a way to indirectly deal with adsorbate--adsorbate interactions.

==== BET Theory for Multilayer Adsorption

_This is an "advanced topic" not discussed in class and provided solely for the interested reader._

The models we have discussed so far assume that there is only a monolayer of adsorbates along the surface.
However, multiple layers of adsorbates that are stabilized by van der Waals interactions are oftentimes possible, particularly at low temperatures and high gas pressures.
To address this limitation, we will introduce Brunauer--Emmett--Teller (BET) theory.
In BET theory, we make the following assumptions:

1. Adsorption occurs on well-defined sites with one molecule adsorbing per surface site.
2. The molecule adsorbed at layer $i$ can itself act as an adsorption site for a gas molecule to form at layer $i+1$. There are no interactions between the gas molecules in a given layer.
3. The uppermost layer of adsorbates is in equilibrium with the gas phase.
4. The heat of adsorption for the first layer is the strongest and constant. The heat of adsorption for the remaining layers can be approximated as the heat of liquefaction.
5. At the saturation pressure, the number of layers approaches infinity, such that it becomes analogous to the surface being surrounded by a fluid phase.

With these assumptions in place, we consider the adsorption of species A onto the surface of a material.
The adsorption of species A onto the bare surface $#ce("*")_0$ yields a new site $#ce("*")_1$:
$ ce("A") + ce("*")_0 eqArrow(k_1,opposite:k_(-1)) ce("*")_1. $
Generalizing this process to multiple layers, we have
$ ce("A") + ce("*")_(i-1) eqArrow(k_i,opposite:k_(-i)) ce("*")_i, $
where $k_i$ refers to the rate constant for formation of layer $i$ (i.e. adsorption onto layer $i-1$), and $k_(-i)$ refers to desorption from layer $i$.
With this numbering scheme, a perfect monolayer would mean that all adsorbates exist on layer $i=1$, for instance.

We will refrain from providing a detailed derivation of the BET isotherm here, as it is mainly useful for determining the surface area of a material rather than kinetic data.
Simply providing the big reveal, the BET isotherm for multi-layer physisorption can be shown to be
$ theta = (c x)/((1-x) (1+ x(c-1))),quad "where" c equiv K_1/K_ell "and" x equiv P/P_0, $<eq:bet>
where $K_ell$ is the equilibrium constant for adsorption and desorption off of a liquid surface of the molecular species, $P$ is the pressure of the adsorbate, and $P_0$ is its vapor pressure.
Generally, $c$ is simply referred to as the BET $c$ constant.
The BET isotherm is most accurate when $P\/P_0$ is between roughly 0.05 and 0.3.
At low pressures, the heterogeneous nature of the surface sites can play a notable role.
At high pressures, nanoscale and microscale irregularities in the surface itself can impact the results.

It is always worth testing out limiting behavior.
In the case of low pressures, it is unlikely for there to be multilayer adsorption, so we should expect the BET isotherm to be analogous to the Langmuir isotherm in this limit.
Indeed, plugging $x<<1$ we get
$ theta approx (c x)/(1 + c x), $
which is the same functional form as the Langmuir isotherm of
$ theta = (K_"ads" P)/(1+ K_"ads" P) $
and is fully equivalent if $c = K_"ads" P_0$.

As previously alluded to, the BET isotherm is typically used for surface area measurements.
This is done by noting that
$ theta = v/v_"m ", $
where $v$ is the volume of the gas adsorbed to the surface and $v_"m "$ is the volume of gas that would be adsorbed if there were precisely a full monolayer of coverage.
This allows us to rewrite #ref(<eq:bet>) slightly as
$ v = (v_"m " c x)/((1-x) (1+ x(c-1))). $
Generally, one will pick a sample to study and carry out an isotherm measurement with a given adsorbate molecule, which is most typically #ce("N2").
The value for $x$ is the independent variable controlled by the experimentalist by changing the pressure of gas introduced to the system.
In modifying $x$, the volume of adsorbed gas $v$ is measured, from which both $v_"m "$ and $c$ can be obtained as fitting parameters for the particular temperature and adsorbate--adsorbent system.

With the value for $v_"m "$ obtained from experiments, the specific surface area of the material (typically reported in $"m "^2\/"g "$), $S_"BET"$, can be computed as
$ S_"BET" = (v_"m " N_"A " alpha)/V  dot 1/m_"adsorbent" , $
where $N_"A "$ is Avogadro's number, $alpha$ is the adsorption cross section of the adsorbate (i.e. the area that a single adsorbate would cover when adsorbed), $V$ is the molar volume of gas at the same conditions as $v_"m "$ was obtained, and $m_"adsorbent"$ is the mass of the adsorbent.
Typically, $alpha$ and $V$ are tabulated quantities, and $m_"adsorbent"$ is readily measurable.

== Surface Reaction Rate Laws <heterogeneous-catalysis-reaction-mechanisms>

=== Single-Site Mechanisms <single-site-mechanisms>

After a reactant has been adsorbed, it can then react.
A single-site mechanism is one in which the site where the reactant is adsorbed is the only one involved in the reaction.
One simple example of this kind of mechanism would be
$ ce("A^*") eqArrow(k_2,opposite:k_(-2)) ce("B^*") $
for the transformation of species A to B on the catalyst surface.
The net rate for the surface reaction ("SR") is as follows:
$
r_"SR" &= k_2 conc("A^*") - k_(-2) conc("B^*").
$<eq:single_site>

At this point, it is worth clarifying some terminology you may see in the literature.
Oftentimes, it can be more convenient to report or simulate data based on fractional coverages rather than concentrations.
If we divide through the above expression by $conc("*")_0$ on both sides, we get
$ r'_"SR" equiv r_"SR"/conc("*")_0 = k_2 theta_ce("A") - k_(-2) theta_ce("B"). $
Notice how the two rate expressions are analogous, with concentrations replaced by fractional coverages.
Here, the rate has been normalized by the concentration of active sites and, therefore, has units of 1/time.
When dealing with the rate of a net reaction, this site-normalized form of the rate is referred to as a turnover frequency (TOF).
The TOF represents the number of reacting molecules per active site and unit time (assuming that all adsorption sites defined by $conc("*")_0$ are all the possible active sites) since it represents the number of times the catalytic active sites have "turned over" a reaction. 
Since TOFs are normalized by the concentration of active sites and most real catalysts have heterogeneity in the active species, the TOF is best thought of as an averaged property.
#footnote[For additional considerations when reporting and interpreting TOFs, refer to F. Schüth, M.D. Ward, J.M. Buriak, "Common Pitfalls of Catalysis Manuscripts Submitted to Chemistry of Materials", _Chem. Mater._, 30, 3599--3600 (2018). More detailed discussion on this topic can be found in S. Kozuch, J.M.L. Martin, "Turning Over Definitions in Catalytic Cycles", _ACS Catal._, 2, 2787--2794 (2012) and the corresponding response G. Lente, "Comment on 'Turning Over Definitions in Catalytic Cycles'", _ACS Catal._, 3, 381--382 (2013).]


=== Dual-Site Mechanisms <reactions-between-two-surface-species>

One can also consider a different type of surface reaction that consists of a reaction involving two surface sites.#footnote[The surface reaction $ce("A^* + * <=> B^* + *")$ would be another type of dual-site mechanism with an analogous solution.]
For instance,
$ ce("A^* + B^*") eqArrow(k_2,opposite:k_(-2)) ce("C^* + D^*"). $<eq:dual_site>
It might be tempting to write that the net rate for the surface reaction is as follows:
$
r_"SR" =^? k_2 conc("A^*") conc("B^*") - k_(-2) conc("C^*") conc("D^*").
$<eq:fake_dual_sr>
However, this is not the case.
Species $"A "^*$ cannot react with species $"B "^*$ unless they are nearest neighbors.
Therefore, we need to account for this in our rate expression, similar how we needed to account for the probability of adjacent sites in our dissociative adsorption example from #ref(<dissociative-adsorption>).

With this knowledge and in analogy with the statistical corrections introduced in #ref(<dissociative-adsorption>),
the rate expression can be given by
$
r_"SR" = (k_2 z conc("A^*") conc("B^*"))/conc("*")_0  - (k_(-2) z conc("C^*") conc("D^*"))/conc("*")_0,
$<eq:sr_dual>
where $z$ is the coordination number of the adsorption site.
#caution[We use a multiplicative factor of $z\/conc("*")_0$ instead of $z\/2 conc("*")_0$ because #ce("A^*") and #ce("B^*") are distinguishable. If the surface reaction takes place between two identical species, we would need to retain the 1/2 factor.]
From here on out, we will lump the $z$ (or $z\/2$) factor, where applicable, into the rate constant for the sake of simplicity:
$
r_"SR" &= (k'_2 conc("A^*") conc("B^*"))/conc("*")_0  - (k'_(-2) conc("C^*") conc("D^*"))/conc("*")_0.
$
If we wanted to write the above expression in terms of a site-normalized rate, we would instead have
$ r'_"SR" equiv r_"SR"/conc("*")_0 = k'_2 theta_ce("A") theta_ce("B") - k'_(-2) theta_ce("C") theta_ce("D").  $
Notice how the above expression is analogous to #ref(<eq:fake_dual_sr>) with concentrations replaced by fractional coverages.
If we had not accounted for the $z\/conc("*")_0$ factor, then this analogy would no longer hold.

=== Reaction with Unbound Species <reaction-with-unbound-species>

A reaction can also occur between an adsorbed molecule and a molecule in the continuum.
When the adsorbed molecule reacts with a molecule in the gas phase, this is typically referred to as an Eley--Rideal reaction.

The general equation is

$ ce("A^* + B") eqArrow(k_2,opposite:k_(-2)) ce("C^*"). $

The net rate for the surface reaction is as follows:

$
r_"SR" &= k_2 conc("A^*") p_("B ") - k_(-2) conc("C^*").
$
This is largely analogous to the typical single and dual-site surface reactions, except that here we are considering the partial pressure of species #ce("B") rather than the concentration of #ce("B^*").
We also do not need to account for lattice statistics since there are no surface site pairs involved.

== Catalytic Reaction Mechanisms

=== LHHW Kinetics: The Pedagogical Case

With our newfound knowledge of the individual steps that take place in a catalytic reaction, we can combine them to produce a kinetic model.
When invoking Langmuirian adsorption behavior with the assumption that adsorbates are randomly distributed on the surface (known as the Hinshelwood assumption), the resulting kinetic models are known as Langmuir--Hinshelwood--Hougen--Watson (LHHW) models.

==== Rate Law Derivation

Consider the simple reaction scheme given by
$ ce("A + *") &eqArrow(k_"ads",opposite:k_"des") ce("A^*") \
ce("A^*") &fwdArrow(k_2) ce("P + *").
$
for the net reaction $ce("A->P")$.
The net reaction rate is given by $r = r_"P "$, and so we will focus on the rate of product production from here.
The rate of product production is given by
$ r_"P " = k_2 conc("A^*"). $<eq:lhhw_sample_rate>

We ultimately want to write our rate without transient intermediates.
If we assume that the adsorption of A is quasi-equilibrated, then
$ k_"ads" p_"A " conc("*") = k_"des" conc("A^*"). $
$ conc("A^*") = K_"ads" p_"A " conc("*"). $<eq:conc_a_star_lhhw>
The quasi-equilibrium approximation is fairly reasonable to invoke here because the surface reaction is expected to be substantially slower than the adsorption or desorption steps.

Additionally, we have the site balance of 
$ conc("*")_0 = conc("*") + conc("A^*"), $
which if we solve for #conc("*") and plug the resulting expression into #ref(<eq:conc_a_star_lhhw>) yields
$ conc("A^*") = K_"ads" p_"A " (conc("*")_0-conc("A^*")) $
$ conc("A^*") +  K_"ads" p_"A " conc("A^*") = K_"ads" p_"A " conc("*")_0 $
$ conc("A^*") =  (K_"ads" p_"A " conc("*")_0)/(1+K_"ads" p_"A "). $
Plugging this into our original rate expression from #ref(<eq:lhhw_sample_rate>) results in
$ r_"P " = (k_2 K_"ads" p_"A " conc("*")_0)/(1+K_"ads" p_"A "). $<eq:co_quasi>
Note that if we chose to write the above expression in terms of a turnover frequency, there would be no $conc("*")_0$ term remaining, which is a common feature of LHHW rate expressions.

#plot[#align(center+horizon)[https://marimo.app/l/25oabe]]


==== Alternate Approach

What if we were not convinced we could invoke the quasi-equilibrium approximation on the reversible adsorption step?
Here, we will show that the pseudo-steady state approach yields a very similar result.

We once again start with our rate of production:
$ r_ce("P") = k_2 conc("A^*"). $
However, instead of invoking an equilibrium condition, we apply the pseudo-steady state approximation on the transient intermediate #ce("A^*") as follows:
$ r_ce("A^*") approx 0 = k_"ads" p_ce("A") conc("*") - k_"des" conc("A^*") - k_2 conc("A^*"). $<eq:pssh_example>
We also have the same site balance of 
$ conc("*")_0 = conc("*") + conc("A^*"). $
Combining our site balance with #ref(<eq:pssh_example>) will ultimately yield
$ conc("A^*") = (K_"ads" p_ce("A") conc("*")_0)/(1 + K_"ads" p_ce("A") + k_2/k_"des"), $
such that
$ r_ce("P") = (k_2 K_"ads" p_ce("A") conc("*")_0)/(1 + K_"ads" p_ce("A") + k_2/k_"des"). $
We can see that the pseudo-steady state solution is identical to the quasi-equilibrium solution when $k_2\/k_"des" << 1 + K_"ads" p_ce("A")$.
This is expected to be the case for the quasi-equilibrium approach since $k_"des"$ must be very high compared to $k_2$ for quasi-equilibrium to be invoked in the first place.

==== Limiting Cases

===== Strong Adsorption <strong-adsorption>

Here, we will explore some limiting cases for $r_ce("P")$ using the quasi-equilibrium form from #ref(<eq:co_quasi>):
$ r_"P " = (k_2 K_"ads" p_"A " conc("*")_0)/(1+K_"ads" p_"A "). $
If A adsorbs strongly to the surface (and/or we are in the high pressure limit of $p_ce("A")$), then we arrive at
$ r_"P " approx k_2 conc("*")_0 quad (K_"ads" p_"A ">>1). $
In this scenario, the apparent reaction order of A is 0 because virtually all the sites are covered in A, such that slight variations in A do not have an appreciable influence on the overall rate.
We know the apparent reaction order is 0 in A from the fact that there is no $p_"A "$ term in the simplified rate law.
We can also see this from the formal definition given in #ref(<eq:apparent_order>):
$ alpha_("A,app") = p_"A " ((diff ln(r))/(diff p_"A "))_(p_i,i!=j) = p_"A " (diff ln(k_2 conc("*")_0))/(diff p_"A ") = 0. $

In this scenario, the apparent rate constant would simply be
#footnote[Since $conc("*")_0$ is typically constant, you could lump this term into $k_"app"$. However, since we typically treat $k$ as being independent of concentrations, it is left out here. Ultimately, this decision is largely arbitrary.]
$ k_"app" = k_2. $
By extension, the apparent activation energy is the activation energy associated with $k_2$, which we will denote $E_("a,2")$.
This too can be derived from the formal definition given in #ref(<eq:apparent_e_a>):
$ E_"app" = R T^2 (diff ln(k_"app"))/(diff T) $
$ E_"app" = R T^2 (diff ln(A_2 e^(- E_("a,2")/(R T))))/(diff T) = R T^2 (diff (A_2 - E_("a,2")/(R T)))/(diff T) = R T^2 (E_("a,2")/(R T^2)) = E_("a,2"). $

===== Weak Adsorption

In the opposite extreme, if A adsorbs very weakly to the surface (and/or we are in the low $p_ce("A")$ limit), then we arrive at
$ r_"P " approx k_2 K_"ads" p_"A " conc("*")_0 quad (K_"ads" p_"A "<<1). $
Here, the apparent reaction order of A is 1.
Additionally, the apparent rate constant would now be
$ k_"app" = k_2 K_"ads". $
We can rewrite the apparent rate constant as
$ k_"app" = A_2 exp(-E_("a,2")/(R T)) exp(-(Delta G_("ads")^std)/(R T)) $
$ k_"app" = A_2 exp(-E_("a,2")/(R T)) exp(-(Delta H_("ads")^std)/(R T)) exp((Delta S_("ads")^std)/(R)) $
$ k_"app" = A_2 exp((Delta S_("ads")^std)/(R)) exp(-(E_("a,2")+ Delta H_("ads")^std)/(R T)). $
As such, we arrive at a functional form of $ k_"app" = A_"app" exp(-E_("a,app")/(R T)), $
where
$ A_"app" = A_2 exp((Delta S_("ads")^std)/(R)), quad quad E_("a,app") = E_("a,2") + Delta H_("ads")^std. $
The same result can be found from the formal definition of the apparent activation energy like was done in #ref(<strong-adsorption>).
These apparent kinetic parameters are particularly useful for interpreting kinetic data obtained from experiments, where the net reaction is the main observable phenomena.

=== Reaction Stoichiometric Numbers

Before continuing on to more complex catalytic mechanisms, it is worth introducing a piece of terminology.
Typically, we propose a mechanism composed of many elementary steps that represent the atomistic details of the catalytic reaction as best as we can imagine.
In this context, the concept of the reaction stoichiometric coefficient $sigma_i$ becomes important.
Simply put, $sigma_i$ is the number of times that the $i$-th reaction needs to occur to yield the net reaction stoichiometry. 

Consider the following proposed reaction scheme for the hydrogenation of ethylene:
$
ce("H2 + 2* &<--> 2 H^*"), quad &sigma_1 = 1\
ce("C2H4 + * &<--> C2H4^*"), quad &sigma_2 = 1\
ce("C2H4^* + H^* &<--> C2H5^* + *"), quad &sigma_3 = 1\
ce("C2H5^* + H^* &<--> C2H6 + 2*"), quad &sigma_4 = 1\
ce("C2H4^* + * &<--> CHCH2^* + H^*"), quad &sigma_5 = 0\
ce("CHCH2^*  &<--> CCH3^*"), quad &sigma_6 = 0
$
for the net reaction #ce("H2 + C2H4 <--> C2H6").
The reaction stoichiometric numbers, $sigma_i$, describe the number of times that reaction step contributes to arrive at the net reaction.
Some steps can have $sigma_i=0$ but still be important for the overall catalytic mechanism.
For instance, even though reaction step 5 does not factor into the net reaction equation, it can still influence the kinetics by decreasing the population of #ce("C2H4^*") species and is potentially worth accounting for in a kinetic model.

If we assume that we have surpassed the induction period beyond which the pseudo-steady state hypothesis can be invoked on adsorbed intermediates, we can write
$
r_ce("H^*") &approx 0 = 2 r_1 - r_3 -r_4 + r_5\
r_ce("C2H4^*") &approx 0 = r_2 - r_3 - r_5\
r_ce("C2H5^*") &approx 0 = r_3 - r_4\
r_ce("CHCH2^*") &approx 0 = r_5 - r_6\
r_ce("CCH3^*") &approx 0 = r_6,
$
where we are defining $r_i$ to be the net rate for the $i$-th reaction step given by $r_i equiv r_(i)^+ - r_(i)^-$.

From this system of equations, we can conveniently write
$
r_1=r_2=r_3=r_4, quad r_5=0, quad  r_6 = 0.
$
This leads us to the following relationship:
$ r_i = sigma_i r, $<eq:rate_stoichs>
where $r$ is the net rate of the overall reaction.
Put another way, just like $r_j\/nu_j$ is a constant for each species and can be used to define the rate of reaction $r$ (provided $nu_j != 0$), for catalytic cycles in pseudo-steady state we can say that $r_i\/sigma_i$ is a constant that defines the rate of reaction (provided $sigma_i != 0$).
#caution[Note the assumptions that came with #ref(<eq:rate_stoichs>).
This relationship is applicable for a reaction cycle (e.g. a catalytic reaction or radical-chain reaction) where the pseudo-steady state approximation is applied on the intermediates.
]

=== LHHW Kinetics: Carbon Monoxide Oxidation

==== Rate Law Derivation

We will consider the following reaction of #ce("CO") and #ce("O2") to produce #ce("CO2"), which takes place on Pd:
$
ce("CO + *") &eqArrow(k_1, opposite:k_(-1)) ce("CO^*"), quad &sigma_1 = 2\
ce("O2 + 2 *") &eqArrow(k_2, opposite:k_(-2)) ce("2 O^*"), quad &sigma_2 = 1\
ce("CO^* + O^*") &fwdArrow(k_3) ce("CO2 + 2 *"), quad &sigma_3 = 2
$
with the net reaction $ce("2 CO + O2 -> 2 CO2")$.
We will also assume that the bimolecular surface reaction of #ce("CO^*") and #ce("O^*") is rate-limiting, which in turn implies that the reversible adsorption steps are quasi-equilibrated.

We know that the rate of reaction can be given by $r = r_ce("CO2")$.
From here, we will focus on the rate of #ce("CO2") production.
Based on our prior discussion of bimolecular surface reactions (refer to #ref(<reactions-between-two-surface-species>)), we can write the rate of #ce("CO2") production as
$ r_ce("CO2") = (k'_3 conc("CO^*") conc("O^*"))/conc("*")_0, $<eq:co_rate>
where we have defined $k'_3 equiv k_3 z$ as a matter of simplicity.

Since #conc("CO^*") and #conc("O^*") cannot be measured directly, we seek to replace these variables in #ref(<eq:co_rate>).
Invoking the quasi-equilibrium condition, such that the rate of adsorption and desorption of the reactants are equal in magnitude, yields
$
r_1 &= 0 = k_1 p_("CO") conc("*") - k_(-1) conc("CO^*")\
r_2 &= 0 = (k_2 z p_ce("O2") conc("*")^2)/(2 conc("*")_0) - (k_(-2) z conc("O^*")^2)/(2 conc("*")_0). 
$
Solving for the adsorbed species concentrations yields
$
conc("CO^*") = k_1/k_(-1) p_("CO") conc("*") = K_1 p_("CO") conc("*")\
conc("O^*") = sqrt(k_2/k_(-2)  p_ce("O2") conc("*")^2) = conc("*") sqrt(K_2 p_ce("O2")). $<eq:co_ads_species>
Now we write out the site balance:
$ conc("*")_0= conc("*") + conc("CO^*") + conc("O^*"). $<eq:co_site_balance>
Plugging #ref(<eq:co_ads_species>) into #ref(<eq:co_site_balance>) yields
$
conc("*")_0 &= conc("*")+ K_1 p_("CO") conc("*") + conc("*") sqrt(K_2 p_ce("O2"))\
conc("*") &= conc("*")_0 / (1+ K_1 p_("CO") + sqrt(K_2 p_ce("O2"))).
$<eq:co_star>
Plugging #ref(<eq:co_star>) into #ref(<eq:co_ads_species>) results in
$
conc("CO^*") &= (K_1 p_("CO") conc("*")_0) / (1+K_1 p_("CO") + sqrt(K_2 p_ce("O2")))\
conc("O^*") &= (conc("*")_0 sqrt(K_2 p_ce("O2"))) / (1+K_1 p_("CO") + sqrt(K_2 p_ce("O2"))).
$<eq:co_final>
Finally, substituting #ref(<eq:co_final>) into #ref(<eq:co_rate>) results in the desired rate expression based on experimental observables:
$ r_ce("CO2") = (k'_3 K_1 p_("CO") conc("*")_0 sqrt(K_2 p_ce("O2"))) / (1+K_1 p_("CO") + sqrt(K_2 p_ce("O2")))^2. $
Once again, you may note that if we chose to write the above expression in terms of a turnover frequency, there would be no $conc("*")_0$ term remaining, which is a common feature of LHHW rate expressions.

#caution[If we did not include the $z\/conc("*")_0$ correction in #ref(<eq:co_rate>), the resulting rate expression at the end of the derivation would have a $conc("*")_(0)^2$ term instead of $conc("*")_0$ in the numerator. In general, the presence of higher-order $conc("*")_0$ terms is a sign that lattice statistics have been neglected.]
// #footnote[For an alternate opinion about the $conc("*")_0$ term in catalytic rate expressions, refer to D. Kiani, I.E. Wachs, "The Conundrum of Pair Sites in Langmuir–Hinshelwood Reaction Kinetics in Heterogeneous Catalysis", _ACS Catal._, 14, 10260--10270 (2024).]

==== Limiting Cases

As a sanity check, we can see that if $p_ce("CO")->infinity$ or $p_ce("O2")->infinity$, then $r_ce("CO2")->0$, which makes sense since both species need to be present on the surface so they can react with one another.

We can also consider what happens in other limiting cases.
For instance, if #ce("CO") binds very strongly such that $K_1$ is sufficiently large, we may arrive at the simplified equation
$ r_ce("CO2") = (k'_3 conc("*")_0 sqrt(K_2 p_ce("O2"))) / (K_1 p_("CO")) quad (K_1 p_ce("CO") >> 1 + sqrt(K_2 p_ce("O2"))), $<eq:rxn_CO>
where the apparent order of CO is -1, and the apparent order of #ce("O2") is +1/2.
The apparent order of -1 in CO makes sense because the surface is nearly covered by CO adsorbates, so increasing CO further will only reduce the overall rate.

Conversely, if #ce("O2") binds very strongly such that $K_2$ is sufficiently large, we may arrive at the simplified equation
$ r_ce("CO2") approx (k'_3 K_1 p_("CO") conc("*")_0) / sqrt(K_2 p_ce("O2")) quad (sqrt(K_2 p_ce("O2")) >> 1+K_1 p_ce("CO")), $
which has CO with an apparent order of +1 but $ce("O2")$ with an apparent order of -1/2, indicating that #ce("O2") is now inhibiting the overall rate, as would be expected.
In each of these limiting cases, it is important to remember that these are not the rate laws themselves but rather what may be observed experimentally for a given set of conditions.


=== Most Abundant Reaction Intermediate

==== Example 1

We will introduce one common approximation in analyzing the kinetics of catalytic mechanisms.
When one adsorbate on the surface is present in great excess, it is referred to as the most abundant reaction intermediate (MARI).
Invoking the MARI approximation can greatly simplify mechanistic analyses.

Consider the following proposed mechanism in the Haber--Bosch process:
$ 
ce("N2 + 2*") &fwdArrow(k_1) ce("2 N^*"), quad &sigma_1 = 1\
ce("H2 + 2*") &eqArrow(K_2) ce("2H^*"), quad &sigma_2 = 3\
ce("N^* + 3H^*") &eqArrow(K_3) ce("NH3 + 4^*"), quad &sigma_3 = 2
$
for the net reaction #ce("N2 + 3 H2 -> 2NH3") where the dissociative adsorption of #ce("N2") is the rate-limiting step, causing the other reactions to be in quasi-equilibrium.
We will also assume that #ce("H^*") is the MARI on the basis of experiments.
From the written expressions, it is clear that the third reaction is not elementary and is instead a sum of many individual surface reactions.
As we will show below, this is perfectly okay if we can invoke the MARI approximation.

We start by writing the rate of reaction based on the rate-limiting step:
$ r = r_1 = (k'_1 p_ce("N2") conc("*")^2)/conc("*")_0, $
We wish to get rid of $conc("*")_0$ as usual, so we will write a site balance.
Here, however, our site balance can be greatly simplified by invoking the MARI approximation:
$ conc("*")_0 = conc("H^*") + conc("*"). $
We have two species here that we wish to get rid of: $conc("H^*")$ and $conc("*")$.
We proceed by invoking quasi-equilibrium on step 2 to arrive at
$ K_2 = (conc("H^*")^2)/(conc("*")^2 p_ce("H2")). $
Solving for $conc("H^*")$ yields
$ conc("H^*") = conc("*") sqrt(K_2 p_ce("H2")), $
where we naturally only take the positive root solution.
We can plug the above expression into our site balance to arrive at
$ conc("*") = conc("*")_0/(1+sqrt(K_2 p_ce("H2"))). $
Plugging the above expression into our rate expression results in 
$ r = (k'_1 p_ce("N2")conc("*")_0) /(1+sqrt(K_2 p_ce("H2")))^2. $
We can see that the rate expression can be written without any transient intermediates and without knowing any particular details about the non-elementary (i.e. third) step in the proposed mechanism since $K_3$ never appears in our rate.
In essence, the MARI approximation allows us to greatly reduce the size of our mechanism.

==== Example 2

Here, we will again consider the Haber--Bosch process with a slightly modified set of reaction equations:
$ 
ce("N2 + 2*") &fwdArrow(k_1) ce("2 N^*"),quad &sigma_1 = 1\
ce("N^* + 3/2 H2") &eqArrow(K_2) ce("NH3 + *"),quad &sigma_2 = 2
$
where the dissociative adsorption of #ce("N2") is rate-limiting.
We will also consider the case where #ce("N^*") is the MARI, perhaps due to the use of a different catalyst than in the previous example.
This example is notably different in that the rate-determining step is also the one involving the MARI.
Clearly, the second equation cannot possibly be an elementary step, but as we will once again demonstrate, this is perfectly acceptable if we are able to invoke the MARI approximation.

The rate of reaction can be expressed based on the rate-determining step as
$ r = r_1 = (k'_1 p_ce("N2") conc("*")^2)/conc("*")_0. $

As usual, we want to get rid of #conc("*") from our rate expression.
We will invoke quasi-equilibrium on the second step since it is fast with respect to the rate-determining step.
Even though we cannot write step 2 using an elementary rate expression, we can still express its equilibrium constant via
$ K_2 = (p_ce("NH3") conc("*"))/(conc("N^*") p_ce("H2")^(3\/2)) $<eq:K2_mari>
as demonstrated in #ref(<k_state_function>).

We also have our site balance, which can be written in simplified form due to the MARI approximation:
$ conc("*")_0 = conc("*") + conc("N^*"). $<eq:mari_site_balance>
Note that we have excluded any other surface species even though there must be some amount of #ce("H^*") on the surface in order to make #ce("NH3").

Solving for #conc("N^*") in #ref(<eq:K2_mari>) yields
$ conc("N^*") = (p_ce("NH3") conc("*"))/(K_2 p_ce("H2")^(3\/2) ). $
and plugging #conc("N^*") into the site balance results in
$ conc("*")_0 = conc("*") + (p_ce("NH3") conc("*"))/(K_2 p_ce("H2")^(3\/2) ) $
$ conc("*") = conc("*")_0/(1 + (p_ce("NH3") )/(K_2 p_ce("H2")^(3\/2))). $

Finally, we can plug the above equation into our rate expression and simplify:
$ r = (k'_1 p_ce("N2")conc("*")_0) /(1 + (p_ce("NH3") )/(K_2 p_ce("H2")^(3\/2)))^2. $

The main conclusion from this exercise is that we do not need any information about intermediate steps in the mechanism or the underlying details of the steps associated with $K_2$.
When invoking the MARI, we were able to write the rate expression using an equilibrium expression and an elementary rate law.

=== Non-LHHW Kinetics

==== Eley--Rideal Mechanism

Consider the proposed mechanism
$
ce("H2") + ce("2 *") &eqArrow(k_1,opposite:k_(-1)) ce("2 H^*")\
ce("2 H^*") + ce("C2H2") &fwdArrow(k_"H ") ce("C2H4") + ce("2 *")
$
with the net reaction #ce("C2H2 + H2 -> C2H4").
#footnote[The second step is an example of a termolecular reaction that is actually quite likely to occur. Since the adsorbates are anchored onto the surface, it is quite natural for #ce("C2H2") to be hydrogenated in this way if it is well-aligned with the two hydrogen adsorbates.]
We will assume that the hydrogenation reaction is rate-limiting, such that the #ce("H2") adsorption is quasi-equilibrated.

Here, we have a reaction between an adsorbed species and gas-phase species (i.e. an Eley--Rideal mechanism).
The rate of product formation, which is identical to the rate of reaction, is given by
$ r = (k'_"H " conc("H^*")^2 p_ce("C2H2"))/(conc("*")_0), $
where $k'_"H " equiv k_"H " z \/2$.
To get rid of the intermediate in our rate expression, we can invoke the quasi-equilibrium assumption on the first step to arrive at
$
(z k_1 p_ce("H2") conc("*")^2)/(2 conc("*")_0) = (z k_(-1) conc("H^*")^2)/(2 conc("*")_0)\
conc("H^*") = sqrt(K_1 p_ce("H2") conc("*")^2) = conc("*") sqrt(K_1 p_ce("H2")).
$
Plugging this into our expression for $r$ yields
$ r = (k'_"H " K_1 conc("*")^2 p_ce("H2")  p_ce("C2H2"))/conc("*")_0.  $
Now we can write our site balance:
$ conc("*")_0 &= conc("*") + conc("H^*") = conc("*") +  conc("*") sqrt(K_1 p_ce("H2"))  = conc("*") (1 + sqrt(K_1 p_ce("H2"))), $
such that
$ conc("*") &= conc("*")_0/(1 + sqrt(K_1 p_ce("H2"))). $
Substituting this into $r$ yields
$
r = (k'_"H " K_1 conc("*")_0 p_ce("H2") p_ce("C2H2"))/(1 + sqrt(K_1 p_ce("H2")))^2.
$
This is essentially a simplified form of the analogous LHHW model.

==== Mars--van Krevelen Mechanism

In some cases, the catalytic adsorption sites can be part of the catalytic cycle itself, such as the formation of a high-energy defect site or vacancy in the lattice that reversible is formed and consumed during the course of the reaction.
This is known as a Mars--van Krevlen cycle.
There are several authoritative references that can be read to learn more about the derivation of the rate in a Mars--van Krevlen mechanism.
We refrain from doing so here simply as a matter of brevity and, in part, because it has been shown that the original derivation has numerous logical inconsistencies that are difficult to justify.
#footnote[M.A. Vannice, "An analysis of the Mars–van Krevelen rate expression", _Catal. Today_, 123, 18--22 (2007).]

==== Limitations of LHHW

Aside from variations on the typical surface reactions invoked in LHHW models, there are many inherent assumptions of LHHW kinetics that may be violated in reality.
LHHW kinetic models inherit all the assumptions associated with the Langmuir model of adsorption, including energetically uniform adsorption sites and a lack of adsorbate--adsorbate interactions.
If this approximation breaks down, additional complexity must be introduced.
Additionally, through the Hinshelwood assumption, the adsorbates are assumed to be randomly distributed on the surface.

A clear demonstration of a failure mode with the LHHW model is related to a phenomenon described as "jamming."
Consider the dissociative chemisorption reaction
$ ce("H2 + 2 *") fwdArrow(k) ce("2 H^*"). $
We know that the turnover frequency for this process can be given by
$ r' = k' p_ce("H2") theta_ce("*")^2. $<eq:jam_rate>
If we treat adsorption as occurring on a 2D lattice, it is possible to have a "jammed lattice" at sufficiently high values of $theta_"H "$ like that shown in #ref(<fig:jammed>).
Here, there are still vacant sites available (i.e. $theta_*>0$).
However, none of these vacant sites can lead to a further reaction because there is no space for two H atoms to adsorb at adjacent sites.
In other words, $r'$ should be zero in the jammed state, but this is inconsistent with #ref(<eq:jam_rate>) for $theta_* > 0$.
Clearly, the Langmuir--Hinshelwood formalism cannot reproduce the jammed state where $theta_ce("*")>0$ but $theta_ce("**")=0$.

#figure(
  image("figures/jammed.jpg", width: 20%),
  caption: [Depiction of a jammed lattice.]
)<fig:jammed>

Of course, this does not necessarily mean a LHHW model cannot yield a sufficiently good fit to experimentally obtained kinetic data.
Rather, it means that the interpretability may be limited.
We refer the interested reader to external sources for further discussions about potential improvements to the LHHW formalism, particularly as it relates to dealing with site ensembles.
#footnote[N.K. Razdan, A. Bhan, "Kinetic description of site ensembles on catalytic surfaces", _Proc. Natl. Acad. Sci. U.S.A._, 118, e2019055118 (2021). Also refer to N.K. Razdan, A. Bhan, "Catalytic site ensembles: A context to reexamine the Langmuir--Hinshelwood kinetic description", _J. Catal._, 404, 726--744 (2021). ]


= Chemical Reactor Archetypes

Previously, we have discussed the kinetics of chemical reactions without discussing the details of the reactor used to run the reactions.
Here, we provide a brief overview of different reactor technologies operating isothermally.
Later in the course, we will revisit the reactor design equations to account for non-isothermal effects. 

== The Mass Balance <conservation-of-mass>

In order to understand the behavior of chemical reactors, we must start with the key governing principle behind it all: the conservation of mass.
Consider a chemical reaction taking place inside a reactor, and focus on a unit volume within the reactor itself.
We can ask ourselves the question: what is the rate of change of species $A_j$ within our control volume?
Species $A_j$ will enter the control volume, exit the control volume, and (potentially) be generated or consumed due to a chemical reaction.
Written in a mathematical form, this can be described as follows:
$ (dif n_j) / (dif t) = dot(n)_(j,0) - dot(n)_(j,1) + G_j, $
where $dif n_j \/ dif t$ is the rate of change of species $A_j$ in the control volume (i.e. its rate of accumulation), $dot(n)_(j,0)$ is the molar flow rate of species $A_j$ into the volume, $dot(n)_(j,1)$ is the molar flow rate of species $A_j$ out of the volume at time $t$, and $G_j$ is the rate of generation of species $A_j$ (i.e. from the chemical reaction).
While somewhat abstract, this equation is simply a restatement of the conservation of mass based on #ref(<fig:reactor_volume>).

#figure(
  image("figures/reactor_volume.svg", width:33%),
  caption:[Schematic of a reactor volume, $V$, with an inlet molar flow given by $dot(n)_(j,0)$, outlet molar flow given by $dot(n)_(j,1)$ and rate of reaction producing $j$ given by $G_j$.]
)<fig:reactor_volume>

The generation term, $G_j$, is of particular importance given that it is the direct consequence of any chemical reactions taking place in the system.
By definition, its value is positive if A is serving as a net product in the chemical reactions taking place, while its value is negative if A is serving as a net reactant being consumed.
Typically, one measures the change in concentration of a species over time, which we have been denoting as $r_j$.
We can express $G_j$ more concretely as follows:
$ G_j = integral r_j dif V $
where $r_j$ is the rate of production of species $A_j$, such that it is positive if more $A_j$ is being generated over the course of the reaction and negative if it is being consumed.

== Batch Reactors <batch-reactors>

A batch reactor is a relatively simple reactor archetype that has no input or output when the chemical reaction is occurring, as shown in #ref(<fig:batch>).
While simple, the batch reactor is the most common reactor archetype when studying the reaction kinetics of liquid-phase reactions.

#figure(
  image("figures/batch.svg", width:15%),
  caption:[Schematic of a constant-volume batch reactor.]
)<fig:batch>


=== Concentration Basis

Without any inputs or outputs, the batch reactor has $dot(n)_(j,0)=dot(n)_(j,1)=0$. As such, the mole balance is 
$ (dif n_j) / (dif t) = integral r_j dif V. $
If the reaction mixture is perfectly mixed (i.e. spatially uniform) so that $r_j$ is independent of position (a common assumption for the batch reactor), then we can state
$ (dif n_j) / (dif t) = r_j V. $
Solving for the rate of reaction of species $A_j$, we see that
$ r_j = 1/V (dif n_j)/(dif t). $<eq:batch_rate>
If we assume that the batch reactor operates with a constant volume for the reaction mixture, as is oftentimes the case, we can write the rate directly in terms of concentration:
$ r_j = (dif [A_j])/(dif t). $ 
We have arrived at the rate expression we have used countless times throughout this course.

=== Conversion Basis

We may also wish to write the rate equation in terms of a fractional conversion.
Recognizing that $X_j = 1 - n_j\/n_(j,0)$ and therefore $n_j= n_(j,0)(1-X_j)$, we can plug this into #ref(<eq:batch_rate>) to get
$ r_j = -n_(j,0)/V (dif X_j) / (dif t). $
In general, one can substitute in the rate law for $r_j$ and integrate in order to find the time to achieve a pre-specified concentration or conversion.
The analytical solution of this form is typically known as a design equation.

== Plug-Flow Reactors <plug-flow-reactors>

A plug-flow reactor (PFR) is a tubular reactor that that has a continuous, flowing stream containing the reaction mixture, as shown in #ref(<fig:pfr>).
The PFR is particularly common when measuring the kinetics of gas-phase reactions.
Generally, solid-catalyzed vapor-phase reactions --- as are common in the field of heterogeneous catalysis --- are carried out with what is known as a packed-bed reactor (PBR), which in an idealized case can be modeled as a PFR.

#figure(
  image("figures/pfr.svg", width:30%),
  caption:[Schematic of a plug-flow reactor.]
)<fig:pfr>

=== Concentration Basis

For a PFR, the design equation can be solved by differentiating the mole balance with respect to volume, but an easier way is to perform a mole balance on species $A_j$ in a differential segment of the reactor volume, $dif V$.

Assuming steady-state conditions, the differential balance can be written as 
$ 0 = dot(n)_(j) - (dot(n)_(j) + dif dot(n)_j) + r_j dif V, $
which is equivalent to
$ r_j = (dif dot(n)_j) / (dif V). $<eq:pbr>

Under the assumption of a constant volumetric flow rate, we can define a property known as the residence time, $tau$, as
$ tau equiv V/dot(v) = (A_"c "z)/dot(v) = z/u, $<eq:residence_time>
where $dot(v)$ is the volumetric flow rate (i.e. $"m "^3$/s), $A_"c "$ is the cross-sectional area of the reactor tube (i.e. $"m"^2$), $z$ is the length of the reactor (i.e. m), and $u$ is the linear velocity of the fluid (i.e. m/s).
The value of $tau$ has units of time and reflects the time a given molecule in the fluid spends inside the reactor and is the natural analogue to the clock time $t$ used in the batch reactor derivation.
We can plug the definition of $tau$ into #ref(<eq:pbr>) to arrive at
$ r_j = (dif [A_j] ) / (dif tau) = u (dif [A_j] ) / (dif z). $<eq:pfr_mass_balance>
As is likely quite apparent, the design equation for a PFR is identical to that of a batch reactor where we have swapped out $t$ for $tau$.

=== Conversion Basis

Recognizing that $X_j = 1 - dot(n)_(j) \/ dot(n)_(j,0)$ and therefore $dot(n)_(j) = dot(n)_(j,0) (1-X_j)$, we can plug this into #ref(<eq:pbr>) expression to get
$ r_j = - dot(n)_(j,0) (dif X_j) / (dif V). $<eq:pfr_r_a>


If the reaction is carried out with a heterogeneous catalyst (as is the case with a packed bed reactor), then the catalyst mass $W$ may be used in place of volume to normalize the rate.
In this case, we have
$ r_j = - dot(n)_(j,0) (dif X_j) / (dif W). $


== Continuous-Stirred Tank Reactors <continuous-stirred-tank-reactors>

The continuous-stirred tank reactor (CSTR) has an inlet and outlet flow of chemicals, as shown in #ref(<fig:cstr>).
CSTRs are typically assumed to be operated near steady state (such that the accumulation term is zero) and are assumed to be perfectly mixed.

#figure(
  image("figures/cstr.svg", width:25%),
  caption:[Schematic of a continuous-stirred tank reactor.]
)<fig:cstr>

=== Concentration Basis

The mole balance for the CSTR can be written as 
$ (dif n_j)/(dif t) = dot(n)_(j,0) - dot(n)_(j) + r_j V, $<eq:cstr_unsteady>
which becomes the following after invoking steady-state conditions:
$ 0 = dot(n)_(j,0) - dot(n)_(j) + r_j V. $
Solving for the reaction rate yields
$ r_j = (dot(n)_(j) - dot(n)_(j,0)) / V. $<eq:cstr>

With the above expression, we aim to write $r_j$ in terms of species concentrations.
To do so, note that for a given flow rate with a spatially uniform concentration across the reactor volume, we have
$ dot(n)_j = [A_j] dot(v), $<eq:volumetric_flow_rate>
where $dot(v)$ is the volumetric flow rate.
Plugging #ref(<eq:volumetric_flow_rate>) into #ref(<eq:cstr>) results in
$ r_j = (([A_j] dot(v)) - ([A_j] dot(v))_0)/V. $
At steady state, the volumetric flow rates in and out of the reactor are identical, such that
$ r_j = (dot(v)([A_j] - [A_j]_0))/V. $
Like with the PFR, we need to think about the time that a given packet of fluid is within the reactor, which we will again refer to as $tau$ as defined by $tau equiv V\/dot(v)$.
In the context of a CSTR, we refer to $tau$ as the space time (or average residence time) since a distribution of residence times are possible.
With our definition of $tau$, we can write the rate expression as
$ r_j =  ([A_j] - [A_j]_0) / tau. $<eq:cstr_mass_balance>
Unlike the batch reactor and PFR, the CSTR design equation contains no derivatives and is merely a simple algebraic equation.

=== Conversion Basis

If we wish to write the CSTR design equation in terms of conversion, we can once again note that $X_j = 1 - dot(n)_(j) \/ dot(n)_(j,0)$ and therefore $dot(n)_(j) = dot(n)_(j,0) (1-X_j)$.
Plugging this into #ref(<eq:cstr>), we arrive at
$ r_j = - (dot(n)_(j,0) X_j ) / V. $<eq:cstr_r_a>


== Comparing Flow Reactors

=== Volume Considerations

We can also make a direct comparison between CSTRs and PFRs.
The conversion-based design equation for the PFR given by #ref(<eq:pfr_r_a>) is extremely similar to that of the CSTR given by #ref(<eq:cstr_r_a>):
$ 
r_j = -dot(n)_(j,0) (dif X_j)/(dif V) quad ("PFR") quad quad r_j = -dot(n)_(j,0) (X_j)/(V) quad ("CSTR")\
V = integral_(0)^X_j -(dot(n)_(j,0))/(r_j) dif X'_j quad ("PFR") quad quad V = -dot(n)_(j,0)/(r_j) dot.op X_j quad ("CSTR").
$
Here, we have written the expressions in terms of the necessary reactor volume to achieve a given conversion, which (when plotted) is known as a Levenspiel plot.
For the PFR, the volume is simply the area under the curve in a Levenspiel plot of $-dot(n)_(j,0)\/r_j$ vs $X_j$.
In contrast, for the CSTR, the volume is the area of a rectangle with height $-dot(n)_(j,0)\/r_j$ and width $X_j$ in a Levenspiel plot.
This is demonstrated in X.
It is also apparent from X that, for a given conversion, a CSTR has a larger required volume than the corresponding PFR.

#figure(
    grid(
        columns: (auto, auto),
        rows:    (auto, auto),
        column-gutter: -8em,
        [ #image("figures/levenspiel_pfr.png",   width: 50%) ],
        [ #image("figures/levenspiel_cstr.png", width: 50%) ],
    ),
    caption: [Levenspiel plots for a PFR (left) and CSTR (right) showing a graphical method to determine the required reactor volume for a given conversion (shaded region).]
)<fig:levenspiel>

When considering several CSTRs in series, the total required volume can be reduced, as shown in #ref(<fig:cstr_series>).
From this comparison, it also becomes immediately clear that an infinite set of CSTRs in series would behave identically to a PFR.


#figure(
  image("figures/levenspiel_cstr_series.png", width:30%),
  caption:[Levenspiel plot for a series of continuous-stirred tank reactors.]
)<fig:cstr_series>

=== Residence Time Considerations

Similarly, we can compare the PFR and CSTR based on the residence time to achieve a specific outlet concentration:
$ r_j = (dif [A_j] ) / (dif tau) quad ("PFR"), quad quad r_j =  ([A_j] - [A_j]_0) / tau quad ("CSTR") $
$ tau = integral_([A_j]_0)^([A_j]) (1) / (r_j) dif [A_j]' quad ("PFR"), quad quad tau = 1/r_j dot.op ([A_j] - [A_j]_0) quad ("CSTR") $
Therefore, we can graphically compare the required $tau$ by making a plot of $1\/r_j$ vs. $[A_j] - [A_j]_0$ in analogy with the Levenspiel plots in #ref(<fig:levenspiel>).

#self[Pg 100 from Lanny. Note the product vs inlet comment from the CSTR. In the CSTR the rectangle is drawn with a height equal to l/r evaluated at the product conversion, while in the PFTR the height varies from inlet to product conversion.]


= Nonisothermal Considerations

It might come as no surprise that many reactions are not operated isothermally.
After all, most reactions involve an appreciable heat of reaction, and it may not always be possible or desirable to operate them with a constant heat exchange rate to maintain a given temperature.

This fact becomes critical to account for when realizing that the rate of reaction varies as a function of temperature.
For instance, consider the constant-volume batch reactor design equation of 
$ r_(j)(T) = (dif [A_j])/(dif t) quad  ("with" (dif T)/(dif t) = 0), $
where we are denoting $r_(j)(T)$ to make it clear that the rate is a function of temperature (e.g. via the Arrhenius equation).
Normally, we would derive an expression for $r_j$ and integrate to identify how the species concentration changes like in #ref(<integrated-rate-expressions>).
Implicitly, this procedure was done under the assumption that we were operating at a given value of $T$.
However, in a non-isothermal reactor, the value of $T$ is not a constant value and will change with time.
Therefore, under non-isothermal conditions we have $ r_(j) (T) = (dif [A_j])/(dif t)  quad  ("with" (dif T)/(dif t) != 0). $
Ultimately, this leads us to a coupled system of differential equations (i.e. the mass and energy balances) that must be solved simultaneously in order to understand how the reaction proceeds with time.

== General Energy Balance

We will begin with the general energy balance
$ (dif E)/(dif t) = accent(m,dot)_0 accent(E,hat)_0 - accent(m,dot)_1 accent(E,hat)_1 + accent(Q,dot) + accent(W,dot), $
where $E$ represents the total energy, $accent(m,dot)$ is a mass flow rate, $accent(E,hat)$ is an energy per unit mass, $accent(Q,dot)$ is the rate of heat added to the system, and $accent(W,dot)$ is the rate of work done on the system.
The work term is defined as
$ accent(W,dot) = accent(W,dot)_"f " + accent(W,dot)_"s " + accent(W,dot)_"b ", $
where the individual terms are for work done by flow streams moving through the reactor, shaft work being done by stirrers, compressors, and other equipment, and work done when moving the system boundary (if moved), respectively.

We define $accent(W,dot)_"f "$ as 
$ accent(W,dot)_"f " = - P Delta dot(v) = dot(v)_0 P_0 -dot(v) P, $
where $dot(v)$ is the volumetric flow rate and $P$ is the pressure of the stream.
We define $accent(W,dot)_"b "$ in differential form as
$ accent(W,dot)_"b " = -P (dif V)/(dif t). $

With this, we will rewrite $accent(W,dot)$ as 
$ accent(W,dot) = dot(v)_0 P_0 -dot(v) P + accent(W,dot)_"s " - P (dif V)/(dif t), $
where we will leave the shaft work as an abstract variable for now since it depends on what equipment is being used.

The total energy of the system is the sum of all internal, potential, and kinetic energies:
$ E = U + "KE" + "PE". $
We can rewrite our derivative as
$ (dif (U + "KE" + "PE"))/(dif t) = accent(m,dot)_0 (accent(U,hat)_0 + accent("KE",hat)_0 + accent("PE",hat)_0) - accent(m,dot) (accent(U,hat) + accent("KE",hat) + accent("PE",hat)) \ + accent(Q,dot) + dot(v)_0 P_0 -dot(v) P + accent(W,dot)_"s " - P (dif V)/(dif t). $
We will now take advantage of the definition of enthalpy, $H = U + P V$, to say that
$
accent(H,hat) =  accent(U,hat) + (P V)/m = accent(U,hat) + P/rho.
$
Now we can state
$
(dif (U + "KE" + "PE"))/(dif t) = accent(m,dot)_0 (accent(H,hat)_0 - P_0/rho_0 + accent("KE",hat)_0 + accent("PE",hat)_0) - accent(m,dot) (accent(H,hat) - P/rho + accent("KE",hat) + accent("PE",hat)) \ + accent(Q,dot)  + dot(v)_0 P_0 -dot(v) P + accent(W,dot)_"s " - P (dif V)/(dif t)
$
Since dimensional analysis will readily indicate that $accent(m,dot) P \/ rho=dot(v) P$, we can simplify our expression a bit to arrive at
$
(dif (U + "KE" + "PE"))/(dif t) = accent(m,dot)_0 (accent(H,hat)_0 + accent("KE",hat)_0 + accent("PE",hat)_0) - accent(m,dot) (accent(H,hat) + accent("KE",hat) + accent("PE",hat)) \ + accent(Q,dot)  + accent(W,dot)_"s " - P (dif V)/(dif t)
$<eq:general_energy_balance>
We will leave our general expression at this for now.

== Batch Reactors
// include drawing

=== General Approach <batch-general>

The batch reactor has no flow streams, so our energy balance collapses down to
$ (dif (U + "KE" + "PE"))/(dif t) = accent(Q,dot) + accent(W,dot)_"s " - P (dif V)/(dif t). $

From here, we will make some assumptions.
The first assumption is that the shaft work is negligible (i.e. $accent(W,dot)_"s "=0$), which is generally true so long as the stirrers and other equipment are not drawing significant power.
The second approximation we will make is that the kinetic energy of the fluid does not appreciably change (i.e. $dif "KE"\/dif t=0$), which is particularly reasonable for a batch reactor but even for flow reactors since the flows themselves are not drastically changing speeds.
The final approximation we will make is that the change in potential energy is negligible (i.e. $dif "PE"\/dif t=0$), which is reasonable for a batch reactor but may not be reasonable for reactors in the presence of external fields (e.g. an electrochemical system).

With this, we have
$ (dif U)/(dif t) = accent(Q,dot) - P (dif V)/(dif t). $

We now will use enthalpy for the rest of our derivation, noting that the differential form is given by
$
dif H &= dif U + P dif V + V dif P.
$<eq:enthalpy_definition>
Substituting in for $dif U$ results in
$
(dif H - P dif V - V dif P)/(dif t) &= accent(Q,dot) - P (dif V)/(dif t)\
(dif H)/(dif t) - V (dif P)/(dif t) &= accent(Q,dot).
$<eq:dH_batch>
For single-phase systems, we can write the total differential of the enthalpy as 
$ dif H = ((diff H)/(diff T))_(P,n_j) dif T + ((diff H)/(diff P))_(T,n_j) dif P + sum_j ((diff H)/(diff n_j))_(T,p,n_(k!=j)) dif n_j. $
The left-most derivative is the definition of the constant-pressure heat capacity:
$ C_"P " equiv ((diff H)/(diff T))_(P,n_j). $

Now for the second term.
We will take advantage of a few thermodynamic relationships here without re-deriving them from scratch.
Namely,
$ ((diff H)/(diff P))_(T,n_j)  = V - T ((diff V)/(diff T))_(P,n_j) = V -  T alpha V = V(1-alpha T), $
where $alpha$ is the coefficient of thermal expansion defined as
$ alpha equiv 1/V ((diff V)/(diff T))_(P,n_j). $

Finally, the last term to deal with is quite simple.
It is simply a restatement of the sum of partial molar enthalpies,
#footnote[Note that a partial molar property is not the same as a molar property. The partial molar enthalpy describes the enthalpy change as a function of the moles of a specific species, whereas the molar enthalpy is an averaged change over all species in the system. This also implies that $sum_j n_j macron(H)_j = H$.]
$accent(H,macron)_j$, where
$ accent(H,macron)_j equiv ((diff H)/(diff n_j))_(T,p,n_(k!=j)). $

Now to put it all together:
$ dif H =  m hat(C)_"P " dif T +  V (1-alpha T) dif P+ sum_j accent(H,macron)_j dif n_j. $<eq:dif_h>
Here, we have chosen to use the specific heat capacity, $accent(C,hat)_"P "$, in place of an extensive heat capacity since the former is a more natural property to measure.
Assuming the density of the fluid is uniform, the mass can be determined quite easily from $m = rho V$ as well.

Plugging the above expression into #ref(<eq:dH_batch>) results in
$
(m hat(C)_"P " dif T +  V (1-alpha T) dif P+ sum_j accent(H,macron)_j dif n_j)/(dif t) - V (dif P)/(dif t) &= accent(Q,dot)\
m hat(C)_"P " (dif T)/(dif t) + V (1-alpha T) (dif P)/(dif t) + sum_j accent(H,macron)_j (dif n_j)/(dif t) - V (dif P)/(dif t) &= accent(Q,dot).\
m hat(C)_"P " (dif T)/(dif t) - alpha T V (dif P)/(dif t) &= - sum_j accent(H,macron)_j (dif n_j)/(dif t) + accent(Q,dot).
$
It may seem like this is still a mess (and to some degree, it is), but we have made a lot of progress.
Every algebraic term in this expression is something that can be experimentally measured or computed, although the trickiest term here is probably the summation term.
We will take care of that now.

We know from the mass balance on the batch reactor (#ref(<eq:batch_rate>)) that
$ (dif n_j)/(dif t) = r_j V. $<eq:batch_r_j>
Here, $r_j$ is on a per-species basis.
For reasons that will become clearer shortly, we can use #ref(<eq:sum_stoichs_rate>) to restate the above expression as
$ (dif n_j)/(dif t) = V sum_i nu_(i,j) r_i. $ <eq:dn_j_batch>
Plugging this expression into our energy balance yields
$ m hat(C)_"P " (dif T)/(dif t) - alpha T V (dif P)/(dif t) &= - V sum_j accent(H,macron)_j sum_i nu_(i,j) r_i + accent(Q,dot). $
We can re-group this a bit to help us out in visualizing a substitution that is about to come:
$ m hat(C)_"P " (dif T)/(dif t) - alpha T V (dif P)/(dif t) &= - V sum_i ( sum_j nu_(i,j) accent(H,macron)_j) r_i + accent(Q,dot). $

If we assume that the partial molar enthalpies are the same as pure component enthalpies, then we can take advantage of the following relationship exists for a given reaction $i$ and species $j$:
$ sum_j nu_(i,j) accent(H,macron)_j=Delta H_("rxn",i), $<eq:partial_molar_enthalpy>
such that
$ m hat(C)_"P " (dif T)/(dif t) - alpha T V (dif P)/(dif t) &= - V sum_i Delta H_("rxn",i) r_i + accent(Q,dot). $<eq:energy_balance_batch_general>
Should we wish to consider a net reaction rather than individual elementary reactions, the above expression can also be rewritten without the summation as
$ m hat(C)_"P " (dif T)/(dif t) - alpha T V (dif P)/(dif t) &= - Delta H_("rxn") r V + accent(Q,dot). $<eq:batch_nonisothermal>

There we have it --- a compact expression for the energy balance in a single-phase batch reactor, which can be coupled with the mass balance to better understand reaction progress.

=== Simplifying Cases

From here, there are many simplifications that can be made depending on the system under investigation.
We will not discuss these derivations at length in class for the sake of time and since the mathematical gymnastics is essentially the same, but the full details are reproduced below for full clarity.

==== Summary

$ m hat(C)_"P " (dif T)/(dif t) = - V sum_i Delta H_("rxn",i) r_i + accent(Q,dot) quad ("constant" P "or incompressible")
$
$ m hat(C)_"V " (dif T)/(dif t)  &= V sum_i (-Delta H_("rxn",i) + alpha/kappa T  Delta V_("rxn",i))  r_i   + accent(Q,dot) quad ("constant" V) $
$ m hat(C)_"V " (dif T)/(dif t)  &= V sum_i (-Delta H_("rxn",i) + R T sum_j nu_(i,j))  r_i   + accent(Q,dot) quad ("constant " V, "ideal gas") $

==== Constant Pressure or Incompressible Fluid

We can now consider some optional simplifications that may or may not apply to a given batch reactor of interest.
If the reactor operates under constant pressure, then we can say $dif P\/ dif t=0$.
If the reactor fluid is incompressible, then $alpha=0$.
In both cases, we have
$ m hat(C)_"P " (dif T)/(dif t) = - V sum_i Delta H_("rxn",i) r_i + accent(Q,dot).
$
This is a typical energy balance when dealing with liquid-phase reactions in a batch reactor.

==== Constant Volume

The constant pressure assumption is likely reasonable for most liquids but is going to be questionable for most gas-phase reactions.
Here, we will derive an expression for a constant volume reactor that may be a bit more suitable for gases.
To start, we will return to our energy balance in #ref(<eq:energy_balance_batch_general>) from before we made any assumptions about constant pressure:
$ m hat(C)_"P " (dif T)/(dif t) - alpha T V (dif P)/(dif t) = - V sum_i Delta H_("rxn",i) r_i + accent(Q,dot). $
Then we will write out the total derivative for pressure in terms of $T$, $V$, and $n_j$ as
$ dif P = ((diff P)/(diff T))_(V,n_j) dif T + ((diff P)/(diff V))_(T,n_j) dif V + sum_j ((diff P)/(diff n_j))_(T,V,n_(k!=j)) dif n_j. $
For constant pressure, we have $dif V =0$, such that
$ dif P = ((diff P)/(diff T))_(V,n_j) dif T + sum_j ((diff P)/(diff n_j))_(T,V,n_(k!=j)) dif n_j. $
Plugging this into our energy balance yields
$
m hat(C)_"P " (dif T)/(dif t) - alpha T V (((diff P)/(diff T))_(V,n_j) dif T + sum_j ((diff P)/(diff n_j))_(T,V,n_(k!=j)) dif n_j)/(dif t) &= -  V sum_i Delta H_("rxn",i) r_i + accent(Q,dot)\
(dif T)/(dif t) (m hat(C)_"P "  - alpha T V ((diff P)/(diff T))_(V,n_j) ) - alpha T V sum_j  ((diff P)/(diff n_j))_(T,V,n_(k!=j)) (dif n_j)/(dif t)   &= -  V sum_i Delta H_("rxn",i) r_i + accent(Q,dot).
$
From here, we will take advantage of a known thermodynamic relationship:
$ m hat(C)_"P " = m hat(C)_"V " + alpha T V ((diff P)/(diff T))_(V,n_j). $
Plugging this into our expression, we have
$ m hat(C)_"V " (dif T)/(dif t) - alpha T V sum_j  ((diff P)/(diff n_j))_(T,V,n_(k!=j)) (dif n_j)/(dif t)   &= - V sum_i Delta H_("rxn",i) Delta r_i + accent(Q,dot). $

This is much simpler, but there is still more to be done here.
We will take advantage of another thermodynamic relationship
$ ((diff P)/(diff n_j))_(T,V,n_(k != j)) = accent(V,macron)_j / (V kappa), $
where $accent(V,macron)_j$ is the partial molar volume of species $j$ and $kappa$ is the isothermal compressibility defined as
$ kappa equiv -1/V ((diff V)/(diff P)). $
Plugging this in, we get
$ m hat(C)_"V " (dif T)/(dif t) - alpha/kappa T  sum_j accent(V,macron)_j  (dif n_j)/(dif t)   &= - V sum_i Delta H_("rxn",i) Delta r_i + accent(Q,dot). $

Plugging in our expression for $dif n_j\/dif t$ from the mass balance in #ref(<eq:batch_r_j>) further simplifies things to
$ m hat(C)_"V " (dif T)/(dif t) - alpha/kappa T V sum_j accent(V,macron)_j sum_i nu_(i,j) r_i  = -  V sum_i Delta H_("rxn",i) Delta r_i + accent(Q,dot), $
which we again re-group as
$ m hat(C)_"V " (dif T)/(dif t) - alpha/kappa T V sum_i (sum_j accent(V,macron)_j  nu_(i,j)) r_i  = -  V sum_i Delta H_("rxn",i) Delta r_i + accent(Q,dot). $

Like we did for the partial molar enthalpy in #ref(<eq:partial_molar_enthalpy>), we will state
$ sum_j nu_(i,j) accent(V,macron)_j  = Delta V_("rxn",i), $<eq:partial_volume_balance>
where $Delta V_("rxn",i)$ is the change in volume over the course of reaction $i$.
Therefore,
$ m hat(C)_"V " (dif T)/(dif t) - alpha/kappa T V sum_i Delta V_("rxn",i) r_i  = -  V sum_i Delta H_("rxn",i) r_i + accent(Q,dot). $
Combining the sums gets us our general but slightly more manageable expression for the energy balance of a constant-volume batch reactor,
$ m hat(C)_"V " (dif T)/(dif t)  &= V sum_i (-Delta H_("rxn",i) + alpha/kappa T  Delta V_("rxn",i))  r_i   + accent(Q,dot). $

==== Ideal Gas at Constant Volume

If our reacting mixture is an ideal gas mixture, we can make some further simplifications by noting that
$ kappa equiv - 1/V ((diff V)/(diff P))_(T,n_j) = -1/V (diff((n R T)/P)/(diff P))_(T,n_j) = 1/V (n R T)/P^2 = 1/V (P V)/P^2 =1/P  $
$ alpha equiv (1/V) ((diff V)/(diff T))_(P,n_j) = 1/V (diff((n R T)/P)/(diff T))_(P,n_j) = 1/V (n R)/P = 1/V (P V)/(P T) = 1/T. $
Plugging these expression in yields
$ m hat(C)_"V " (dif T)/(dif t)  &= V sum_i (-Delta H_("rxn",i) +  P  Delta V_("rxn",i))  r_i   + accent(Q,dot). $
Finally, we can note that 
$ Delta V_("rxn",i) = (Delta n_("rxn",i) R T)/P, $
such that
$ m hat(C)_"V " (dif T)/(dif t)  &= V sum_i (-Delta H_("rxn",i) + R T sum_j nu_(i,j))  r_i   + accent(Q,dot), $<eq:batch_energy_balance_ideal>
where we took advantage of the fact that $Delta n_("rxn",i) = sum_j nu_(i,j)$.

=== Example Application of the Energy Balance <example-batch-energy>

==== Defining and Solving a System of ODEs

Since it may appear mostly as an abstract concept up until now, it is worth taking a step back to think about how we might apply the energy balance in practice.

Consider the gas-phase, elementary reaction of
$ ce("A") fwdArrow(k) ce("2B") $
that takes place in a well-mixed, adiabatic batch reactor at constant volume without the constraint of isothermal operation.
For the sake of simplicity, we will consider the gases to behave ideally.
Naturally, we might wish to understand how the temperature of the reactor changes as a function of time.

To answer this question, we start by writing the typical mass balance for species A as
$ r_ce("A") = (dif conc("A"))/(dif t). $
Since the reaction is elementary, we also know the rate expression can be given as:
$ r_ce("A") = -k conc("A"), $
such that
$ (dif conc("A"))/(dif t) = -k conc("A"). $
Thus far, this is essentially the same procedure we have done many times before, such as in #ref(<integrated-rate-expressions>).
However, we cannot proceed with the integration yet since $k(T)$ and $T(t)$ for non-isothermal operation.

We now move onto the simplified energy balance from #ref(<eq:batch_energy_balance_ideal>) to state
$ rho hat(C)_"V " (dif T)/(dif t)  &= (-Delta H_("rxn") + R T) r, $
where we note that $sum_j nu_j = 1$ for the given reaction, $dot(Q)=0$ for an adiabatic process.
We know that $ r = - r_"A " = k conc("A"), $
such that
$ m hat(C)_"V " (dif T)/(dif t)  &= (-Delta H_("rxn") + R T) k conc("A") V. $
This leaves us with a system of differential equations that must be solved simultaneously:
$ 
(dif conc("A"))/(dif t) = -k conc("A"), quad (dif T)/(dif t)  = ((-Delta H_("rxn") + R T) k conc("A") V)/(m hat(C)_"V "),
$<eq:batch_simultaneous>
To make matters even clearer, we can substitute in the Arrhenius equation for $k$ to state
$ 
(dif conc("A"))/(dif t) = -A' exp(-E_"a "/(R T)) conc("A"), quad (dif T)/(dif t)  = ((-Delta H_("rxn") + R T) A' exp(-E_"a "/(R T)) conc("A") V)/(m hat(C)_"V "),
$
where $A'$ is the pre-exponential factor (to distinguish it from species A).

From this set of equations, we can see that both expressions depend on #conc("A") and $T$, both of which evolve with $t$.
For the case where $Delta H_"rxn"<0$, we see that $dif T\/dif t>0$, such that the temperature will increase continually until all of species A is consumed.
Clearly, there needs to be some amount of temperature control in order for the reactor operation to be safe and industrially viable.

Generally, these systems of ordinary differential equations (ODEs) can only be solved numerically.
The details of numerical methods are beyond the scope of this course, but it is still useful to have a high-level understanding of what is involved in setting up such equations.
#footnote[Scenarios like this, where you understand the scientific context but may need assistance on how to begin carrying out the computation, can be great use cases for large language models. Pasting an image of #ref(<eq:batch_simultaneous>) into ChatGPT 4o with the prompt "Write me a Python script that will solve this system of ODEs. Note that k(T)." yields an example script that is nearly perfect (in this case, the Arrhenius equation was simply missing the factor of $R$ in the denominator of the exponential).
]

#plot[https://marimo.app/l/gygake]

==== Temperature-Dependence for the Enthalpy

For the sake of simplicity, it is sometimes assumed that the temperature dependence of $Delta H_"rxn"$ is weak over the relatively limited range of operating temperatures.
To relax this somewhat questionable assumption, one can typically use an empirical relationship known as the Shomate equation, which takes the following form:
$ C_"P " = A + B T + C T^2 + D T^3 + E/T^2, $
where each parameter is determined based on statistical regression to experimental data.
If the different components in a fluid behave ideally, the fluid's heat capacity is simply the sum of its individual component heat capacities.
This is useful to know, as pure component heat capacities are tabulated in many places, such as the _NIST Chemistry WebBook_. 

From this, we can express the temperature-dependence for enthalpy as
$ H(T_2) - H(T_1) = integral_(T_1)^(T_2) C_"P " dif T $
$ H(T_2) - H(T_1) = A Delta T + (B (Delta T)^2)/2 + (C (Delta T)^3)/3 + (D (Delta T)^4)/4 - E/(Delta T), $
which comes from the definition of the constant-pressure heat capacity, $C_"P "$.
Since $Delta H_"rxn"$ is a state function, we can equivalently use $Delta H_"rxn"$ in the above expression in place of $H$.
The value for $C_"V "$ in the energy balance can also depend on temperature, albeit much less so than $Delta H_"rxn"$.

== Plug Flow Reactors

=== Energy Balance

For the sake of brevity, we will forego a detailed derivation of the PFR energy, especially since it is very similar to that of the batch reactor (as was observed for the mass balance).
#footnote[For a thorough derivation of reactor energy balances, refer to Chapter 6 of _Chemical Reactor Analysis and Design Fundamentals_ by J.B. Rawlings and J.G. Ekerdt.]
Instead, in analogy with the single-phase batch reactor energy balance given by #ref(<eq:batch_nonisothermal>), we will simply state that single-phase PFR energy balance is
$ rho hat(C)_"P " (dif T)/(dif tau) + (1-alpha T) (dif P)/(dif tau) = - sum_i Delta H_("rxn",i) r_i + dot(Q)/V. $
Assuming the cross-sectional area, $A_"c "$, of the PFR is constant, then we can equivalently write the derivatives in terms of $z$ instead of $V$ via a simple change of variables (i.e. using the relationship of $tau = z \/u $ from #ref(<eq:residence_time>)):
$ rho u hat(C)_"P " (dif T)/(dif z) + rho u (1-alpha T) (dif P)/(dif z) = - sum_i Delta H_("rxn",i) r_i + dot(Q)/V, $
where $u$ is the linear velocity of the fluid.

If the pressure drop along the reactor is negligible or the fluid is an ideal gas mixture (i.e. $alpha T =1$), then $dif P\/dif tau = dif P\/dif z = 0$, simplifying the energy balance further:
$ rho u hat(C)_"P " (dif T)/(dif z) = - sum_i Delta H_("rxn",i) r_i + dot(Q)/V quad ("negligible pressure drop or ideal gas"). $<eq:pfr_nonisothermal_ideal>
Alternatively, if the fluid is incompressible (i.e. $alpha=0$) as is commonly assumed for liquid-phase reactions, then
$ rho u hat(C)_"P " (dif T)/(dif z) + u (dif P)/(dif z) = - sum_i Delta H_("rxn",i) r_i + dot(Q)/V quad ("incompressible"). $

=== Hot Spots and Thermal Runaway

We will now revisit the example carried out with the batch reactor in #ref(<example-batch-energy>) with a few modifications.
Consider again the gas-phase, elementary reaction of 
$ ce("A") fwdArrow(k) ce("2B") $
where A and B behave ideally.
We now decide to consider a PFR with constant cross-sectional area (rather than a batch reactor with constant volume).
Once again, we will not make any assumptions about isothermal operation.
As was shown in the batch reactor example, the temperature will continually rise if $Delta H_"rxn"<0$ and $dot(Q) = 0$, so we will make one other modification: we will surround the PFR with a heat transfer fluid that allows for the possibility of heat transfer between the reacting fluid and its surroundings (i.e. $dot(Q) != 0$).
As a reminder, the goal will be to write a coupled set of mass and energy balances for the PFR.

Qualitatively similar to the batch reactor example, from the PFR mass balance given by #ref(<eq:pfr_mass_balance>) and the simplified energy balance given by #ref(<eq:pfr_nonisothermal_ideal>), we can state
$ (dif conc("A"))/(dif tau) = -k conc("A"), quad rho hat(C)_"P " (dif T)/(dif tau) = -Delta H_"rxn" k conc("A") + dot(Q)/V. $
The $dot(Q)$ term is what can prevent a continual increase in temperature over the reactor length for an exothermic reaction, provided $dot(Q)<0$.
The highest temperature position along the reactor length is termed the hot spot.

If $Delta H_"rxn"$ is highly exothermic, the hot spot can spike to extremely high temperatures very quickly if one is not careful.
The exothermic reaction causes an increase in temperature, which itself increases the rate of reaction.
If the rate of heat loss to the surroundings via $dot(Q)$ is not sufficiently large, then the reaction will go to completion extremely quickly, raising the temperature to dangerously high values.
Clearly, robust temperature control is a necessity when dealing with non-isothermal reactors.
This not a niche point --- such considerations are just as relevant for even the simplest of reactors like a reaction flask in the lab!

#plot[https://marimo.app/l/v1cm6a]

== Continuous-Stirred Tank Reactors

=== Energy Balance

To derive the expression for the CSTR energy balance, we start again from the general energy balance given by #ref(<eq:general_energy_balance>), neglecting the kinetic and potential energy terms as well as the shaft work:
$ (dif U)/(dif t) = accent(m,dot)_0 accent(H,hat)_0 - accent(m,dot) accent(H,hat) + accent(Q,dot) - P (dif V)/(dif t). $
Using the definition of enthalpy (#ref(<eq:enthalpy_definition>)), we can restate the above expression as
$ (dif H)/(dif t) - V (dif P)/(dif t) - P (dif V)/(dif t) = accent(m,dot)_0 accent(H,hat)_0 - accent(m,dot) accent(H,hat) + accent(Q,dot) - P (dif V)/(dif t), $
which simplifies to
$ (dif H)/(dif t) - V (dif P)/(dif t) = accent(m,dot)_0 accent(H,hat)_0 - accent(m,dot) accent(H,hat) + accent(Q,dot). $

Like the batch reactor example, we can consider a single-phase system where the enthalpy changes due to temperature, pressure, and the moles of species $j$.
Plugging #ref(<eq:dif_h>) into the above expression yields
$ (m hat(C)_"P " dif T + V(1- alpha T) dif P + sum_j accent(H,macron)_j dif n_j)/(dif t)  - P (dif V)/(dif t) = accent(m,dot)_0 accent(H,hat)_0 - accent(m,dot) accent(H,hat) + accent(Q,dot), $
which simplifies to
$ m hat(C)_"P " (dif T)/(dif t) - alpha T (dif P)/(dif t) + sum_j accent(H,macron)_j (dif n_j)/(dif t) = accent(m,dot)_0 accent(H,hat)_0 - accent(m,dot) accent(H,hat) + accent(Q,dot). $ <eq:cstr_energy_unsteady>

We know from the mass balance on the CSTR (#ref(<eq:cstr_unsteady>)) that 
$ (dif n_j)/(dif t) = dot(n)_(j,0) - dot(n)_(j) + r_j V, $
or equivalently via #ref(<eq:sum_stoichs_rate>):
$ (dif n_j)/(dif t) = dot(n)_(j,0) - dot(n)_(j) + V sum_i nu_(i,j) r_i. $
Plugging the CSTR mass balance into #ref(<eq:cstr_energy_unsteady>) and following the same approach taken in the batch reactor derivation from #ref(<batch-general>) results in 
$ m hat(C)_"P " (dif T)/(dif t) - alpha T (dif P)/(dif t) = - V sum_i Delta H_("rxn",i) r_i + sum_j dot(n)_(j,0) (accent(H,macron)_(j,0) - accent(H,macron)_(j)) + accent(Q,dot). $

From here, we can start applying our typical approximations.
For instance, we can invoke steady-state conditions to arrive at
$ 0 = - V sum_i Delta H_("rxn",i) r_i + sum_j dot(n)_(j,0) (accent(H,macron)_(j,0) - accent(H,macron)_(j)) + accent(Q,dot). $
To make matters a bit simpler, we can represent the partial molar enthalpies in terms of partial molar heat capacities via
#footnote[Naturally, this requires that the heat capacity does not change between the inlet and outlet.]
$ macron(H)_(j) - macron(H)_(j,0) = macron(C)_("P ",j) (T-T_0), $
such that
$ 0 = - V sum_i Delta H_("rxn",i) r_i + sum_j dot(n)_(j,0) macron(C)_("P ",j) (T_0 - T) + accent(Q,dot). $
Leveraging the definition of a partial molar property, we can state
$ sum_j dot(n)_(j,0) macron(C)_("P ",j) = dot(n)_0 c_("P ") = dot(m)_0 hat(C)_"P ", $
such that we can rewrite the energy balance as
$ 0 = - V sum_i Delta H_("rxn",i) r_i + dot(m)_(0) hat(C)_("P ") (T_0 - T) + accent(Q,dot). $
If we wish to express the above energy balance in terms of $tau$, we can divide through by $V$ and use $dot(v)_0 = dot(m)_0 rho $ to arrive at
$ 0 = - sum_i Delta H_("rxn",i) r_i + (rho hat(C)_("P ") (T_0 - T))/tau + accent(Q,dot)/V. $

<eq:cstr_steady_energy>

=== Multiple Steady States

While #ref(<eq:cstr_steady_energy>) look slightly simpler than the batch and PFR energy balances since it is a series of algebraic expressions rather than differential equations, there is still quite interesting behavior that can emerge.
Namely, unlike other reactor archetypes, CSTRs are known to exhibit multiple physically plausible steady state solutions.

Consider a CSTR operated adiabatically and at steady-state conditions with the following single-phase, exothermic reaction:
$ ce("A") fwdArrow(k) ce("B"). $

The energy balance can be concisely written using #ref(<eq:cstr_steady_energy>) to state
$  0 = - Delta H_("rxn") r V  + (rho hat(C)_("P ") (T_0 - T))/tau. $
Similarly, we know from #ref(<eq:cstr_mass_balance>) that the mass balance for the CSTR can be written as
$ r_ce("A") = (conc("A") - conc("A")_0)/tau $
for a constant volumetric flow rate.
Substituting in our elementary rate law of $r = - r_ce("A") =  k conc("A")$, we arrive at the following system of equations:
$ 0 = - Delta H_("rxn") k conc("A")  + (rho  hat(C)_("P ") (T_0 - T))/tau, quad 0 = (1+ k tau) conc("A") - conc("A")_0. $

This system of non-linear equations is a bit deceptive.
Depending on the initial guess one uses and the reaction conditions themselves, different solutions can potentially be found, each of which may be physically sound and equally valid.
In these scenarios, the CSTR exhibits multiple steady states, and small perturbations to the reaction conditions can trigger drastic changes in the reaction, resulting in an unstable reactor.

#plot[https://marimo.app/l/ebu1s6]

= Transition State Theory <transition-state-theory>

In the previous sections, we have made extensive use of the rate constant, $k$.
Here, we seek to provide theoretical insights into what factors dictate the value of $k$ while also providing an atomistic justification for the functional form of the Arrhenius equation.

== Setting the Stage <setting-the-stage>

=== Rate in Terms of an Equilibrium Constant

We seek to write an expression for the rate constant, $k$, in terms of quantities that can be readily computed from theory and/or experimentally measured.
Transition state theory (TST) is what makes this possible.
We will start by considering the following reaction:
#footnote[We have chosen a bimolecular reaction here simply for demonstration purposes, but the results that follow are not inherently restricted to a particular molecularity if proper care is taken.]
$ ce("A + B") &fwdArrow(k) ce("P") $
In transition state theory, we assume that there is some transient complex (the transition state) that connects the reactants and products along the minimum energy pathway of the reaction coordinate.
This means we can rewrite our reaction as
#footnote[Here, we are tacitly assuming that there is no recrossing, such that we are considering a single isolated event such that P cannot convert back to the reagents. Naturally, it is possible in practice for a product to return to the transition state if enough energy is supplied, but we consider this as a separate event altogether. In practice, recrossing events will decrease $k$ from transition state theory, and this is corrected in a _post hoc_ manner via the transmission coefficient $kappa$ discussed shortly.]
$
ce("A + B") eqArrow(K_"C "^ddagger) ce("AB")^ddagger --> ce("P").
$<eq:tst_rxn>
The net rate of reaction can be represented as
$ r = nu^ddagger conc("AB")^ddagger, $<eq:nu_dagger>
where $nu^ddagger$ is the frequency (in units of $"time "^(-1)$) associated with the vibrational mode along the reaction coordinate that connects the transition state to the product and $conc("AB")^ddagger$ is the concentration of the transition state species.
The concentration of the transition state species, however, is not an observable quantity since the transition state itself is fleeting.
To take care of this challenge, we assume that the transition state is in quasi-equilibrium with the reactants, such that we can return to the definition of the equilibrium constant: 
$ K_"C "^ddagger = conc("AB")^ddagger / (conc("A") conc("B")). $<eq:tst_kc>
Solving for $conc("AB")^ddagger$, we can arrive at
$ r = nu^ddagger K_"C "^ddagger conc("A") conc("B"). $<eq:tst_rate_kc>
This should look like strikingly familiar in an abstract kind of way.
If we compare this to our typical rate expression for a elementary reaction, $r = k conc("A") conc("B")$, it becomes self-evident that
$ k = nu^ddagger K_"C "^ddagger. $<eq:k_nu>
The question now is where to go from here.
We will later show that $nu^ddagger  approx k_"B " T\/h$.
Before then, however, we need to figure out what to do with $K_"C "^ddagger$.

=== Rate in Terms of Molecular Partition Functions

The most pressing situation to address in our definition of $k$ is $K_"C "^ddagger$.
Thankfully, with a healthy dose of statistical thermodynamics, this becomes relatively manageable.
Although it will not be derived here, from statistical mechanics it is known that the equilibrium constant can be expressed in terms of molecular partition functions as follows:
$ K_"a " equiv product_j a_(j)^nu_j  = product_j Z_(j)^(nu_i) exp(- (Delta E^ddagger) / (R T)), $<eq:k_a_partition_functions>
were $Z_j$ is the (unitless) molecular partition function for species $j$ and $Delta E^ddagger$ is the change in electronic energy between the transition state and reactant(s).
#footnote[The reason for the $exp(-Delta E^ddagger\/R T)$ term will become clearer when we introduce the electronic partition function.]

We will describe how one determines a molecular partition function in a moment, but before we do, recall that we have been dealing with concentrations and $K_"C "^ddagger$.
As such, we will instead use
$ K_"C "^ddagger = 1/(N_"A "^(1-m)) (Z'^ddagger) / (Z'_"AB" Z'_"C ") exp(- (Delta E^ddagger) / (R T)), $
where $Z'_j$ is the molecular partition function per unit volume.
The need for $Z'_j$ being in units of $"volume"^(-1)$ is so that we arrive at the appropriate units for $K_"C "^ddagger$.
The factor of $1\/N_"A "^(1-m)$, where $m$ is the molecularity (i.e. $m = 2$ for this example),
#footnote[We could have equally used $delta^ddagger$ in place of $1-m$, where where $delta^dagger$ is the change in stoichiometric numbers between the transition state and reactants. Since we are only focusing on a single transition state-producing event, we use for $1-m$ simplicity.]
is included simply as a means of ensuring that $K_"C "^ddagger$ is in molar units since the partition functions are defined on a per-molecule basis.

It may seem that we have not made much progress here because we do not yet know how to calculate $Z'_j$. Not to worry though, as that will be addressed below.

=== Thermochemistry from Partition Functions <thermochemistry-from-partition-functions>

It should be noted that statistical mechanics allows us to determine thermodynamic properties like the internal energy $U$, enthalpy $H$, entropy $S$, and Gibbs free energy $G$  of the translational, rotational, vibrational, and electronic partition functions can also be used in terms of the partition function.
For the interested reader, we refer you to external sources on the subject matter.
#footnote[Refer to "Section 10.3: Ensemble Properties and Basic Statistical Mechanics" in _Essentials of Computational Chemistry: Theories and Models_ (2#super[nd] ed.) by C.J. Cramer.]
That said, a brief summary is as follows:
$ U &= k_"B " T^2 ((diff ln(Z)) / (diff T))_(N,V) $
$ H &equiv U + P V $
$ S &= k_"B " ln(Z) + k_"B " T ((diff ln(Z))/ (diff T))_(N,V) $
$ G &equiv H - T S, $
where the above holds for the single-particle partition function $Z$.
For $N$ indistinguishable atoms or molecules, one should instead use $Z^N\/N!$ in place of $Z$.

== Contributions to the Partition Function

The total molecular partition function for a species can be broken down into the products of vibrational, rotational, translational, and electronic partition functions.
Written mathematically,
$ Z' = z_"trans"/V z_"rot" z_"vib" z_"el", $<eq:total_partition_function>
where the $z$ values are all unitless.
As we will show below, the natural place to introduce the units of inverse volume in $Z'$ is with the translational partition function.
These individual contributions can also be used to define the translational, rotational, vibrational, and electronic contributions to the thermodynamic state functions as well.

=== Translational Partition Function

We now must define each of the partition functions.
The translational partition function derived from the particle-in-a-box model in quantum chemistry is typically approximated as
$ z_"trans" = V ((2 pi m k_"B " T)/h^2)^(3/2) = V / Lambda^3, quad Lambda equiv h / sqrt(2 pi m k_"B " T) $
where $V$ is a reference volume,
$m$ is the mass of the molecule, $h$ is Planck's constant, and $Lambda$ is the thermal de Broglie wavelength.

When dealing with gases, one can equivalently use a standard-state pressure $P^std$ (typically taken as 1 bar) in place of $V$, such as by invoking the ideal gas law: $V = k_"B " T\/P^std$.
This would ultimately ensure that our rate constant is expressed with pressure-based units rather than concentrations.
#caution[It must be emphasized that $V$ is describing the volume used to normalize the wavefunction within the particle-in-a-box model.
In other words, it is best thought of as a reference volume for bookkeeping purposes and is not necessarily the volume of the reactor.
In return, $P^std$ in the translational partition function is the standard state pressure and is not the same as the operating pressure.
]

It is also worth noting that $z_"trans"$ can be generalized to an arbitrary set of $d$ dimensions (e.g. in the case of a system that does not have all three translational degrees of freedom).
To do so, it is as simple as stating
$ z_"trans" = (L / Lambda)^d, $
where $L$ is now a reference length dimension.

=== Rotational Partition Function

Moving onward, we will now investigate the rotational partition function.
The rotational partition function is derived based on the rigid--rotor quantum-mechanical model and is different depending on the shape of the molecule.
Generally, it can be approximates as follows:
$
z_"rot" &= 1 quad ("monatomic")\
z_"rot" &= (8 pi^2 I k_"B " T) / (sigma h^2) = T/(sigma Theta_"rot"), quad Theta_"rot" equiv h^2/(8 pi^2 I k_"B ") quad ("linear")\
z_"rot" &= (8 pi^2 (8 pi^3 I_1 I_2 I_3)^(1/2) (k_"B "T)^(3/2)) / (sigma h^3) = 1/sigma ((pi T^3)/(Theta_(1,"rot") Theta_(2,"rot") Theta_(3,"rot")))^(1/2) quad ("nonlinear"),
$
where $Theta_("rot")$ is the characteristic rotational temperature.
In these equations, $sigma$ represents the rotational symmetry number and is determined by the number of spatial orientations of the subject molecule that are identical.
For instance, $sigma$ is a value of 2 for linear molecules with a center of symmetry (e.g. a homonuclear diatomic molecule) and 1 for linear molecules without a center of symmetry (e.g. a heteronuclear diatomic molecule).
The quantity $I$ is the moment of inertia, and for the nonlinear case they are the three principal moments.
The moment of inertia is defined as
$ I equiv sum_j M_j r_(j)^2 $
where $M_j$ is the mass of atom $j$ and $r_j$ is the distance of atom $j$ to the axis of rotation.

=== Vibrational Partition Function

The vibrational partition function is derived based on the harmonic-oscillator quantum-mechanical model and is given by
$ z_"vib" =  product_(j=0)^N (exp(- (h nu_j)/(2 k_"B " T)))/(1 - exp(- (h nu_j)/(k_"B " T))) = product_(j=0)^N (exp(- Theta_(j,"vib")/(2 T)))/(1 - exp(- (Theta_(j,"vib"))/T)), quad Theta_(j,"vib") equiv (h nu_j)/k_"B " $
where the product is taken over all vibrational modes in the system, $N$ is the number of vibrational modes, $nu_j$ is the $j$-th vibrational frequency, and $Theta_(j,"vib")$ is the characteristic vibrational temperature.
It should be noted that vibrational spectra are normally reported in units of wavenumbers ($"cm"^(-1)$), $accent(nu,tilde)$.
To convert a wavenumber to a frequency, the following relationship can be used: $nu_j = c accent(nu,tilde)_j$.

The number of vibrational modes can be determined as follows:
#footnote[For transition states, one of the $N$ vibrational modes is imaginary. As we will justify shortly, only the real vibrational modes should be included in calculating $z_"vib"$. This mode will be accounted for with $nu^ddagger$.]
$
N=0 quad ("monatomic")\
N=3N_0 - 5 quad ("linear")\
N=3N_0 - 6 quad ("nonlinear").
$
For pedagogical purposes, it is helpful to rewrite the vibrational partition function slightly as
$ z_"vib" =  exp(-E_"ZPVE" / (k_"B " T)) product_(j=0)^N 1/(1 - exp(- (Theta_(j,"vib"))/T)), quad E_"ZPVE" equiv 1/2 sum_j h nu_j, $<eq:vib_part>
where $E_"ZPVE"$ is the zero-point vibrational energy.
#tip[You may see some people use $Delta U^ddagger$ in place of $Delta E^ddagger$ in the expression for $K_"C "^ddagger$. If this is done, it necessarily implies that the $exp(-E_"ZPVE"\/k_"B " T)$ term was factored out of the vibrational partition function and instead included as part of $Delta U^ddagger$ since $U equiv E + E_"ZPVE"$.]

=== Electronic Partition Function

Finally, the electronic partition function is given by
$ z_"el" = sum_j g_i exp(- E_j / (k_"B " T) ) $
where $g_j$ is the degeneracy of electronic state $j$ and $E_j$ is the electronic energy for electronic state $j$.
As is typically done, we will adopt the ground-state (i.e. $j=0$) as the zero-energy reference point, such that all values of $E_j$ are taken with respect to $E_0$.
#tip[This is where the $exp(-Delta E^ddagger\/R T)$ term comes from in our expression for $K_"C "^ddagger$. It is because we have factored out the energy of the electronic ground state for each species from $z_"el"$ so we could have the ground state contribution to $z_"el"$ be equal to $g_0$.]
For the sake of simplicity, excited states (i.e. $j>=1$) are often assumed to have a negligible contribution to $z_"el"$ due to their high energies with respect to the ground state.
In this case, we can simply state
$ z_"el" = g_0. $

The degeneracy, $g_j$, is analogous to the spin multiplicity, defined as the number of unpaired electrons plus one.
For instance, #ce("CH4^∙") has one unpaired electron and therefore has $g_0=2$ (i.e. the ground-state has a degeneracy of 2).
Similarly, most non-radical species have $q_"el" approx 1$.
However, this is not universally true.
For instance, the ground-state magnetic configuration of #ce("O2") has two unpaired electrons, such that $g_0=3$ (i.e. the ground-state is a triplet).

== Rates of Reaction from Partition Functions <rates-of-reaction>

=== The Idealized Case

With the partition function business out of the way, let's revisit our expression for the concentration-based rate constant:
$ K_"C "^ddagger = 1/(N_"A "^(1-m)) (Z'^ddagger) / (Z'_"AB" Z'_"C ") exp(-(Delta E^ddagger )/ (R T)). $
We now know how to compute the partition functions, which is a relief.
The main ingredient we need to either compute or measure are the vibrational modes of the system (and the spin multiplicity or excited states, if relevant).

Revisiting our rate expression from #ref(<eq:tst_rate_kc>), we have
$
r &= nu^ddagger K_"C "^ddagger conc("A") conc("B")\
r &= nu^ddagger 1/(N_"A "^(1-m)) (Z'^ddagger) / (Z'_"AB" Z'_"C ") exp(- (Delta E^ddagger) / (R T)) conc("A") conc("B").
$<eq:rate_tst_intermediate>

We are still left to figure out what do we do about $nu^ddagger$.
The motion along the minimum energy pathway through the transition state is along a vibrational mode representing the bond-breaking or bond-making event.
As alluded to previously, $nu^ddagger$ can be thought of as a vibrational frequency describing this event.
We know from the definition of the vibrational partition function (#ref(<eq:vib_part>)) that a single vibrational mode can be expressed as
$ z_"vib, TS mode" = (1 - exp(- (h nu^ddagger) / (k_"B " T)))^(-1) approx (k_"B "T) / (h nu^ddagger). $
where the latter approximation is made because $h nu^ddagger << k_"B " T$ in most cases.
#footnote[We have implicitly taken advantage of the Taylor expansion $exp(x) = 1 + x + x^2\/2! + x^3\/3! + ...$ and dropped the second-order and higher terms.]
With some rearrangement, we have
$ nu^ddagger = (k_"B " T) / h 1/ z_"vib, TS mode" $
This expression implies that we can state $nu^ddagger = k_"B " T \/ h$ if we remove this one (imaginary) vibrational mode from the expression for $z_"vib"^ddagger$, which we have already done per convention.

By plugging our result into #ref(<eq:rate_tst_intermediate>), we arrive at
$
r &= (k_"B " T)/h K_"C "^ddagger conc("A") conc("B")\
r &= (k_"B "T)/h 1/(N_"A "^(1-m)) (Z'^ddagger) / (Z'_"AB" Z'_"C ") exp(- (Delta E^ddagger) / (R T)) conc("A") conc("B"),
$<eq:tst_final>
or more generally as
$ r &= (k_"B "T)/h 1/(N_"A "^(1-m)) (Z'^ddagger) / (product_(j,nu_(j)<0) Z'_(j)^(|nu_j|)) exp(- (Delta E^ddagger) / (R T)) product_(j,nu_(j)<0) [A_j]^(|nu_j|). $

From the above expression, we can see that
$
k = (k_"B " T)/h K_"C "^ddagger
$
or, equivalently, in terms of the partition functions,
$
k &= A exp(-(Delta E^ddagger)/(R T))\
A &equiv (k_"B " T)/h 1/(N_"A "^(1-m)) (Z'_("AB")^ddagger) / (Z'_"A " Z'_"B ").
$<eq:tst_a>
Thinking back to the modified Arrhenius expressions with temperature-dependence terms on the prefactor as in #ref(<eq:arrhenius_mod>), we can see the $T^n$ dependence of the prefactor has $n=0$ for the empirical Arrhenius expression and $n=1$ from transition state theory.
In practice, however, the temperature effects of this $T$ factor are often negligible when comparing $k$ values at different temperatures, as the exponential term is the main dominating factor.

=== Accounting for Thermodynamic Non-Idealities

In the derivations of the transition state theory rate constant thus far, we have implicitly neglected any thermodynamic non-idealities.
Now it is time for us to revisit our expressions with a fresh mindset.
Recall that we started off our transition state theory derivation with 
$ r = (k_"B " T)/h conc("AB")^ddagger, $
as originally shown in #ref(<eq:nu_dagger>).
Instead of substituting in for $K_"c "^ddagger$ to get rid of the $conc("AB")^ddagger$ intermediate, we will instead take advantage of the definition of $K_"a "^ddagger$ given by #ref(<eq:k_a_k_c_relationship>):
$ K_"a "^ddagger = (1/C^std)^(1-m) (gamma^ddagger conc("AB")^ddagger)/(gamma_"A " conc("A") gamma_"B " conc("B")). $
Solving for $conc("AB")^ddagger$ yields
$ conc("AB")^ddagger = (C^std)^(1-m) K_"a "^ddagger (gamma_"A " gamma_"B ")/gamma^ddagger conc("A") conc("B"). $
Plugging this into our rate expression now yields
$
r &= (k_"B " T)/h (C^std)^(1-m) K_"a "^ddagger (gamma_"A " gamma_"B ")/gamma^ddagger conc("A") conc("B")\
r &= (k_"B " T)/h (C^std)^(1-m) Z^ddagger/(Z_"A " Z_"B ") exp(- (Delta E^ddagger)/(R T)) (gamma_"A " gamma_"B ")/gamma^ddagger conc("A") conc("B"),
$
or more generally as
$ r = (k_"B " T)/h C^std Z^ddagger/(Z_"A " Z_"B ") exp(- (Delta E^ddagger)/(R T)) 1/gamma^ddagger product_(j,nu_j<0) a^(|nu_j|).
$<eq:tst_activities>

Here, the definition of $K_"a "^ddagger$ was substituted in from #ref(<eq:k_a_partition_functions>).
Note that the partition functions (i.e. $Z$) here are their unitless values, not normalized by the reference volume (i.e. $Z'$).
The above expression is the non-ideal analogy to #ref(<eq:tst_final>).

From the above expression, we can see that
$ k_"nonideal" = (k_"B " T)/h (C^std)^(1-m) K_"a "^ddagger (gamma_"A " gamma_"B ")/gamma^ddagger $<eq:k_nonideal>
or, equivalently, in terms of the partition functions,
$ 
k_"nonideal" = A_("nonideal") exp(-(Delta E^ddagger)/(R T))\
A_("nonideal") equiv  (k_"B " T)/h (C^std)^(1-m) Z^ddagger/(Z_"A " Z_"B ") (gamma_"A " gamma_"B ")/gamma^ddagger
$<eq:tst_nonideal>
in analogy with #ref(<eq:tst_a>).

Comparing the non-ideal rate constant in #ref(<eq:tst_nonideal>) with the idealized case in #ref(<eq:tst_a>),
#footnote[Note that $1\/ N_"A "^(1-m) product_j (Z'_j)^(nu_j)$ in the definition of $k_"ideal"$ is equivalent to $(C^std)^(1-m) product_j (Z_j)^(nu_j)$ in the definition of $k_"nonideal"$ since $Z'_j equiv Z_j\/V$.]
we see that the difference in the rate when accounting for thermodynamic non-idealities can be traced back to the ratio of activity coefficients via the following relationship:
$ k_"nonideal" = k_"ideal" (gamma_"A " gamma_"B ")/(gamma^ddagger). $<eq:k_relationship>
#caution[As can be seen from #ref(<eq:k_relationship>), a rate law of the form $r = k conc("A") conc("B")$ cannot simply be rewritten as $r = k a_"A " a_"B "$ to account for thermodynamic non-idealities, as doing so would omit the importance of $gamma^ddagger$.]

A natural question to ask at this point is what value this analysis can provide if it relies on $gamma^ddagger$, which cannot be readily determined from experiments.
Naturally, one answer is that it is possible --- like with other aspects of the transition state theory representation of $k$ --- to calculate $gamma^ddagger$ from quantum-mechanical calculations.
Perhaps more importantly, however, is that #ref(<eq:k_relationship>) can be used in a qualitative way to rationalize the effects of non-idealities.

For instance, in the liquid-phase, if a solvent selectively stabilizes a given species, the activity coefficient for that species will likely be greater than 1.
From a qualitative standpoint, this is because the effective concentration (i.e. activity) of the solute will necessarily be higher since the solute will be less accessible given that it is highly solvated.
Therefore, we can say that if we stabilize the reactants via solvation, then $k_"nonideal"$ will increase.
In contrast, if our reaction media selectively stabilizes the transition state, then $k_"nonideal"$ will decrease.
Similar arguments can be made about gas-phase reactions by considering the fugacity coefficient $phi_j$ in place of the activity coefficient $gamma_j$.

== Applications to Elementary Processes

=== Degrees of Freedom

As was shown previously, the rate constant for a given elementary step is related to the ratio of partition functions for the transition state and the reactant(s).
If one considers a free molecule (e.g. in the gas phase), it has three translational degrees of freedom, three rotational degrees of freedom if nonlinear or two rotational degrees of freedom if linear, and several vibrational degrees of freedom depending on the number of atoms $N_0$.
We will represent this as follows, where the exponents in quotes are simply how many degrees of freedom there are.#footnote[We have tacitly assumed that the excited states are energetically negligible, such that $z_"el" = g_0$ and therefore $z_"el"$ has only one degree of freedom.]
$ Z' = z_"trans"^((3))/V z_"rot"^((m)) z_"vib"^((N)) z_"el" $
where
$
m=0, quad N &=0 quad ("monatomic")\
m=2, quad N &= 3N_0-5 quad ("linear")\
m=3, quad N &=3N_0-6 quad ("non-linear").
$

For a transition state, one vibrational degree of freedom is lost, such that
$ Z'^ddagger = z_"trans"^(ddagger(3))/V z_"rot"^(ddagger(m)) z_"vib"^(ddagger(N-1)) z_"el"^(ddagger) $

With this, we can often make order-of-magnitude arguments and simplifications via
$ k prop Z'^ddagger/(Z'_"AB" Z'_"C "), $
which we will do below.
Note, however, that one can always simply use the transition state theory expression for $k$ in its full form, calculating each partition function without further simplifications (provided the underlying data is accessible).

=== Adsorption

// draw energy diagram

When a molecule adsorbs onto a surface, some of these degrees of freedom are lost.
If the adsorbate is strongly chemisorbed onto the surface, then there are likely no translational or rotational degrees of freedom left.
The vibrational degrees of freedom are still present, although the vibrational modes are likely to differ substantially from the gas phase, and there are now $3N_0$ modes.
These approximations are often known as the harmonic limit.

If the adsorbate is somewhat weakly bound, then the remaining degrees of freedom are likely to be somewhere between that used for a free gas and that of a chemisorbed species.
More complicated expressions are available to model these intermediate behaviors, such as the hindered translator--hindered rotor model.
#footnote[L.H. Sprowl, C.T. Campbell, L. Árnadóttir, "Hindered Translator and Hindered Rotor Models for Adsorbates: Partition Functions and Entropies," _J. Phys. Chem. C_, 120, 9719--9731 (2016).]

For the sake of simplicity, let us assume that an adsorbate binds strongly to the catalyst surface via chemisorption.
The transition state for this process is, by definition, partway between the gas-phase species and the adsorbed species, such that the transition state itself has some intermediate number of motional degrees of freedom remaining.
In other words, the transition state is a semi-mobile species on or near the surface.

Starting from #ref(<eq:tst_a>), if we assume the gas-phase molecule only loses one translational degree of freedom at the transition state, we can write
$ k_"ads" = (k_"B " T)/h  (z_"trans"^(ddagger (2))/V z_"rot"^(ddagger (3)) z_"vib"^(ddagger (N-1)) z_"el"^(ddagger))/(z_"trans"^((3))/V z_"rot"^((3)) z_"vib"^((N)) z_"el"), $
where the numerator refers to the transition state partition functions, and the denominator refers to the partition functions of the gas-phase species before adsorption.
Note that the $exp(- Delta E^ddagger)\/R T$ term is no longer present because $Delta E^ddagger=0$ for non-activated adsorption processes
#footnote[By this, we mean that we are referring to the binding of the adsorbate to the surface without breaking any chemical bonds.] and the $1\/N_"A "^(1-m)$ term is gone because $m=1$ for this process.
#footnote[Admittedly, it is perhaps of questionable logic to invoke transition state theory when there is, formally, no activation barrier. It is perhaps better to think about this as $E_"a "->0$.]

If we make several assumptions, namely that the rotational, vibrational, and electronic partition functions do not appreciable change between the gas-phase and the transition state upon adsorption, then we arrive at
$ k_"ads" = (k_"B " T)/h z^(ddagger (2))_"trans"/z^((3))_"trans". $
Certainly, this is a lot cleaner looking.
Substituting in for the definition of the translational partition functions yields
$ k_"ads" = (k_"B " T)/h (L\/Lambda)^2/(L\/Lambda)^3 = (k_"B "T)/h Lambda/L = (k_"B " T)/(L sqrt(2 pi m k_"B " T)). $
Assuming we have an ideal gas, we can invoke the ideal gas law in the form of
$ P = (k_"B " T)/V. $
With this, we obtain what is known as the Hertz--Knudsen equation:
$ k_"ads" = (P A)/(sqrt(2 pi m k_"B " T)), $
where $A$ is a reference area obtained from $V\/L$.
Here, $P$ is specifically the (partial) pressure of the gaseous species being adsorbed, and $A$ is generally taken as the surface area of the adsorption site.
Sometimes, an empirical sticking coefficient $s$ will also be included in the numerator to account for the probability that the adsorbate will stick to the surface.

Before continuing, it must be emphasized that the Hertz--Knudsen equation is not universal for all adsorption properties.
While it holds for many systems, if the adsorption process breaks any of the aforementioned assumptions (e.g. loss of one translational mode only, ideal gas behavior), one must use transition state theory directly or re-derive a new analytical expression.

=== Desorption

// draw energy diagram

We now shift our focus to desorption, which is simply the reverse process of adsorption.
Unlike adsorption, however, there is a barrier for desorption (it is the heat of desorption).
With desorption, we are going from an adsorbed state to the free gas, with a transition state somewhere in the middle.
If we assume that the adsorbed state has no translational or rotational degrees of freedom and that two translational degrees of freedom and all three rotational degrees of freedom are restored at the transition state (i.e. the reverse of the process described in the derivation of the Hertz--Knudsen equation), then we have
$ k_"des" = (k_"B " T)/h  (z_"trans"^(ddagger (2))/V z_"rot"^(ddagger (3)) z_"vib"^(ddagger (N-1)) z_"el"^(ddagger))/(z_"trans"^((0))/V z_"rot"^((0)) z_"vib"^(ddagger (N)) z_"el"), $
which we will simplify to
$ k_"des" = (k_"B " T)/h (z_("trans")^(ddagger (2)) z_("rot")^(ddagger (3)) z_("vib")^(ddagger (N-1))z_("el")^(ddagger))/(z_("vib")^((N))z_("el")) exp(- (Delta E_"des")/(R T)). $

From here, we will assume that the change in the vibrational partition function is negligible compared to the change in the translational partition functions and that the change in electronic partition function is also negligible (both of which are almost always going to be true).
With this, we can state
$ k_"des" = (k_"B " T)/h z_("trans")^(ddagger (2)) z_("rot")^(ddagger (3)) exp(- (Delta E_"des")/(R T)). $
From here, we plug in our definitions of the partition functions.
We will start with the translational partition function:
$ k_"des" = (k_"B " T)/h (L/Lambda)^2 z_("rot")^(ddagger (3)) exp(- (Delta E_"des")/(R T)) $
$ k_"des" = (k_"B " T)/h (A (2 pi m k_"B " T))/h^2  z_("rot")^(ddagger (3)) exp(- (Delta E_"des")/(R T)). $
At this point, we need to have some information about the shape of our transition state (i.e. unimolecular, linear, or non-linear).
For the sake of simplicity, let's continue our derivation assuming a linear transition state.
$ k_"des" = (k_"B " T)/h (A (2 pi m k_"B " T))/h^2 T/(sigma Theta_"rot")  exp(- (Delta E_"des")/(R T)). $
Repeating this process for a transition state of different geometry is simply a matter of plugging in a different expression for $z_("rot")^ddagger$.

=== Surface Reaction

For surface reactions, differences in the translational and rotational degrees of freedom for both the reactant(s) and transition state are typically negligible.
Formally, we can make an even stronger statement that the translational and rotational degrees of freedom are zero, such that we have
$ k_"SR" = (k_"B " T)/h  (z_"trans"^(ddagger (0))/V z_"rot"^(ddagger (0)) z_"vib"^(ddagger (N-1)) z_"el"^(ddagger))/(z_"trans"^((0))/V z_"rot"^((0)) z_"vib"^((N)) z_"el") exp(- (Delta E^ddagger)/(R T)). $
We can now simplify to
$ k_"SR" = (k_"B " T)/h  (z_"vib"^(ddagger (N-1)) z_"el"^(ddagger))/(z_"vib"^((N)) z_"el") exp(- (Delta E^ddagger)/(R T)). $
Personally, this is about as far as I am willing to simplify things in most cases without further information.

== A Thermodynamic Perspective

Previously, we derived an expression for $k$ based on a statistical mechanics approach (i.e. using partition functions).
We will show in this subsection that an analogous expression can be derived from simple thermodynamic arguments, albeit without the atomistic details.

=== The Eyring Equation <thermodynamic-perspective>

We know from thermodynamics that
$ Delta G^std^ddagger &= - R T ln(K_("a ")^ddagger), $
such that
$ K_"a "^ddagger = exp(-(Delta G^std^ddagger)/ (R T)). $
Plugging $K_"a "^ddagger$ into our expression for $k_"nonideal"$ in #ref(<eq:k_nonideal>) yields
$ k_"nonideal" = (k_"B " T)/h (C^std)^(1-m) exp(-(Delta G^std^ddagger)/ (R T)) (gamma_"A " gamma_"B ")/gamma^ddagger. $<eq:k_dg>
Naturally, when taking advantage of the thermodynamic relationship
$ Delta G^std = Delta H^std - T Delta S^std $
this becomes
$ k = (k_"B "T )/h C^std^(1-m) exp(( Delta S^std^ddagger) / R) exp(-(Delta H^std^ddagger) / (R T)) (gamma_"A " gamma_"B ")/gamma^ddagger. $ <eq:eyring-final>
In the thermodynamically ideal case (i.e. $gamma_"A "=gamma_"B "=gamma^ddagger = 1$), this expression is known as the Eyring equation.

The net rate of reaction also be written out in full as
$ r = (k_"B "T )/h C^std^(1-m) exp(( Delta S^std^ddagger) / R) exp(-(Delta H^std^ddagger) / (R T)) (gamma_"A " gamma_"B ")/gamma^ddagger conc("A") conc("B"), $<eq:eyring_final_rate>
or more generally as
#footnote[In contrast with statements made in G. Lente, "Facts and Alternative Facts in Chemical Kinetics: Remarks About the Kinetic Use of Activities, Termolecular Processes, and Linearization Techniques", _Curr. Opin. Chem. Eng._, 21, 76--83 (2018), rate expressions can indeed be expressed using activities rather than concentrations, as demonstrated throughout this section.]
$ r = (k_"B "T )/h C^std exp(( Delta S^std^ddagger) / R) exp(-(Delta H^std^ddagger) / (R T)) 1/gamma^ddagger product_(j,nu_j<0) a_(j)^(|nu_j|). $
For the sake of illustration, we can separate out the various terms in $k$ in analogy with the Arrhenius equation to arrive at
$
k &= A_0 exp(- (Delta H^std^ddagger) / (R T))\
A_0 &equiv (k_"B " T)/h C^std^(1-m) exp((Delta S^std^ddagger)/R) (gamma_"A " gamma_"B ")/gamma^ddagger.
$

=== The Transmission Coefficient

It is worth noting that in practice, a multiplicative correction factor $kappa$, known as the transmission coefficient, is often included in the expression for $k$.
For instance, it can be used to correct the Eyring equation representation of the rate constant via
$ k = kappa (k_"B " T)/h C^std^(1-m) exp(- (Delta H^std^ddagger) / (R T)) exp((Delta S^std^ddagger)/R) $
in the thermodynamically ideal case.
The value for $kappa$ is between 0 and 1, representing the probability that the vibrational motion given by $nu^ddagger$ pushes the transition state forward towards the products as opposed to backwards towards the reactants.
#footnote[In contrast with statements made in J.F. Perez-Benito, "Some Considerations on the Fundamentals of Chemical Kinetics: Steady State, Quasi-Equilibrium, and Transition State Theory", _J. Chem. Educ._, 94, 1238--1246 (2017), $k$ cannot be proportional to $kappa(1-kappa)$ otherwise $r->0$ as $kappa->1$ and there becomes an unphysical maximum in the rate at $kappa=1\/2$.]
In other words, the rate constant (and rate) from transition state theory is generally an upper-estimate, even if all variables in the uncorrected rate expression were computed with perfect accuracy.

=== Relationship with Activation Energy <relationship-with-activation-energy>

We can also ask how the apparent activation energy commonly reported in experiments from an Arrhenius plot is related to $Delta H^std^ddagger$.
First, we recall the definition of the activation energy from #ref(<eq:apparent_e_a>):
$
E_"a " &= R T^2 (diff ln(k))/(diff T).
$
Plugging in $k$ from the Eyring equation into the above expression yields
$
E_"a " &= R T^2 (diff ((k_"B "T )/h C^std^(1-m) exp(( Delta S^std^ddagger) / R) exp(-(Delta H^std^ddagger) / (R T)))) / (diff T)
$
$ E_"a " &= R T^2 ((diff ln((k_"B ")/h))/(diff T) + (diff ln(T))/(diff T) + (diff ln(C^std^(1-m)))/(diff T) + (diff ((Delta S^std^ddagger)/R))/(diff T) - (diff ((Delta H^std^ddagger)/(R T)))/(diff T)) $
$
E_"a " &= R T^2 (0 + 1/T + (diff ln(C^std^(1-m)))/(diff T) + 0 + (Delta H^std^ddagger) / (R T^2)) $
$
E_"a " &= Delta H^std^ddagger + R T + R T^2((diff ln(C^std^(1-m)))/(diff T)).
$
At this point, we pause.
What we do from here depends on what system we are studying.
If we are studying a solid or a liquid, we might reasonably state that $C^std$ is independent of temperature.
In this case, we end up with
$ E_"a " = Delta H^std^ddagger + R T quad ("solids, liquids"). $
However, if we are studying a gas, then we cannot make the assumption that $C^std$ is independent of temperature.
Assuming the ideal gas law holds, we instead have
$
E_"a " &= Delta H^std^ddagger + R T + R T^2 ((diff ln((P / (R T))^(1-m)))/(diff T))\
E_"a " &= Delta H^std^ddagger + R T + R T^2 ((m-1)/T)\
E_"a " &= Delta H^std^ddagger + m R T quad ("ideal gas").
$<eq:e_app_tst>

In both cases, the derivation has implicitly assumed that $Delta H^std^ddagger$ and $Delta S^std^ddagger$ are independent of temperature, which is only true if we are considering relatively small temperature differences.
Clearly, #ref(<eq:e_app_tst>) indicates there are several possible meanings for a "kinetic barrier."
Therefore, one should take care in clarifying which property is being reported.
#footnote[For a discussion on the relationship between the apparent activation energy and enthalpy in multi-step reactions with multiple transition states, refer to Z. Mao, C.T. Campbell, "Apparent Activation Energies in Complex Reaction Mechanisms: A Simple Relationship via Degrees of Rate Control", _ACS Catal._, 9, 9465--9473 (2019).]

== de Donder Relations

=== Reaction Affinity and Reversibility

In the formulation of transition state theory, we derived the forward directional rate from reactant(s) to product(s) via the intermediate formation of an activated complex.
It is worth thinking about how we can now leverage this information to describe reversible reactions.
#footnote[For a thorough review on this topic, refer to N.K. Razdan, T.C. Lin, A. Bhan, "Concepts Relevant for the Kinetic Analysis of Reversible Reaction Systems", _Chem. Rev._, 123, 2950--3006 (2023).]
One of the most fundamental properties of a reversible elementary reaction is its reversibility, introduced in #ref(<eq:reversibility>)
and repeated here as
$z_i equiv r_(i)^- \/ r_(i)^+.$

To make things a bit simpler to follow for those who do not enjoy gratuitous product notation as much as me, we will invoke an arbitrary reaction: #ce("A + B <--> P").
Assuming we are dealing with directional rates in an elementary reaction, we know that
$ z_i = r_(i)^- / r_(i)^+ = (k_(i)^- conc("P"))/(k_(i)^+ conc("A") conc("B")). $
From the (non-idealized) Eyring equation in #ref(<eq:eyring_final_rate>), we know that
$ k_(i)^+ &= (k_"B " T)/h (C^std)^(-1) exp(-(G^std^ddagger - G_"A "^std - G_"B "^std)/ (R T)) (gamma_"A " gamma_"B ")/gamma^ddagger\
k_(i)^- &= (k_"B " T)/h exp(-(G^std^ddagger - G_"P "^std)/ (R T)) (gamma_"P ")/gamma^ddagger. $

Therefore,
$ k_(i)^- / (k_(i)^+) &= C^std exp((G_"P "^std - G_"A "^std - G_"B "^std) / (R T)) gamma_"P "/(gamma_"A " gamma_"B ")\
k_(i)^- / (k_(i)^+) &= C^std exp((Delta G_i^std) / (R T)) gamma_"P "/(gamma_"A " gamma_"B ").
$<eq:de_donder_deriv1>

That takes care of part of our expression for the reversibility.
For the latter part, we know that the ratio of concentrations is related to the equilibrium constant.
Technically, we have made no assumptions about being at equilibrium here.
As such, we will refer to our ratio of concentrations as a reaction quotient:
$
Delta G_i &= Delta G_i^std + R T ln(Q_"a ")\
Delta G_i &= Delta G_i^std + R T ln(product_j a_(j)^(nu_(i,j))).
$<eq:dg_nonstandard>
where $Delta G_i$ is at the reaction conditions (i.e. not necessarily standard state) and $Q_"a "$ is the activity-based reaction quotient, computed in an analogous manner as the activity-based equilibrium constant but without a restriction of being at equilibrium
We can convert between activities and concentrations via #ref(<eq:activity1>) to arrive at
$
Delta G_i = Delta G_i^std + R T ln((gamma_"P " conc("P")/C^std)/(gamma_"A " conc("A")/C^std gamma_"B " conc("B")/C^std) )\
conc("P")/(conc("A") conc("B")) = 1/C^std (gamma_"A " gamma_"B ")/gamma_"P " exp((Delta G_i-Delta G_i^std)/(R T))
$<eq:de_donder_deriv2>

Now we can use #ref(<eq:de_donder_deriv1>) and #ref(<eq:de_donder_deriv2>), which thankfully simplifies very cleanly to
$ z_i = r_(i)^- / r_(i)^+ = exp((Delta G_i^std) / (R T)) exp((Delta G_i-Delta G_i^std)/(R T)) = exp((Delta G_i) /(R T)). $
By convention, we will define 
$ cal(A)_i equiv -Delta G_i, $
where $cal(A)_i$ is known as the reaction affinity of the $i$-th reaction step.
With this, we have
$ z_i = r_(i)^- / r_(i)^+ = exp(-(cal(A)_i)/(R T)). $<eq:de_donder>
The same result will hold regardless of the elementary reaction we chose to start with or if we neglected thermodynamic non-idealities.
#ref(<eq:de_donder>) is known as the de Donder relation, which is a thermodynamic relationship between the forward and reverse rates of reaction
Admittedly, there is a clear parallel between the de Donder relation and
$ K_"a " = k_(i)^+ / k_(i)^- = exp(- (Delta G^std)/(R T)). $
That said, it must be stressed once more that the de Donder relation is computed from the reaction conditions, not at standard state.

We can see that $z_i<1$ (i.e. an elementary step proceeds in the forward direction) only if $cal(A)_i>0$ and vice versa.
Another way to frame this is as
$ cal(A)_i (r_(i)^+-r_(i)^-) >=0, $
which is known as de Donder's inequality.
For full clarity, there is no direct relationship between $G_i^std$ and the viability of the net reaction proceeding in a given direction.

=== Ruling Out Reaction Mechanisms

==== Closed Cycles

We can use the de Donder relations as a way to immediately rule out physically impossible reaction mechanisms.
For instance, consider the cyclic reaction scheme in Figure XYZ.
#self[draw triangle like in Neil's review.]
Like with any set of state functions in a closed cycle, we know that
$ sum_i A_i = 0. $
However, it is impossible for this statement to be true unless $A_i=0$ for each step.
At least one of the reaction steps must have $A_i<0$ in order for $sum_i A_i = 0$, which violates de Donder's inequality and implies that such a reaction scheme is not viable as written.
Put another way, via de Donder's equality, we know that one of the reaction steps must actually proceed in the reverse direction, breaking the cyclic nature of the proposed mechanism.

==== Thermodynamic Coupling <thermo-coupling>

We can also leverage de Donder relations in non-cyclic reactions.
Consider the following proposed reaction sequence:
$ 
ce("H2 &<--> 2H^∙") quad A^std_(1) = -331 "kJ/mol"\
ce("2H2 + O2 &<--> 2 H2O") quad A^std_(2) = 385 "kJ/mol".
$
for the following overall reaction:
$ ce("3H2 + O2 <--> 2H2O + 2H^∙"). $

For the sake of demonstration, consider the reaction taking place at standard state conditions, such that $A_i=A^(std)_i$.

The de Donder inequality holds for net reaction sequences as well, provided we account for stoichiometry via
$ cal(A) (r^(+)-r^(-)) >=0, quad cal(A) equiv sum_i sigma_i A_i. $
Since $sum_i sigma_i cal(A)_i>0$, one might naively think that the net reaction proceeds in the forward direction, made possible via thermodynamic coupling between the two steps to yield an overall $cal(A)>0$.
However, having $cal(A)>0$ is a necessary but not sufficient condition.
When looking at the individual steps, we see that the first step has $cal(A)_i<0$, making it impossible for it to proceed in the forward direction.
Clearly, the proposed mechanism cannot proceed as-written even though the net reaction has $cal(A)>0$.

==== Kinetic Coupling

What if we considered the same mechanism in #ref(<thermo-coupling>) but relaxed the assumption that we are operating at standard state conditions?
In this scenario, $cal(A)_i != cal(A)^std_i$, and even though $cal(A)^std_1<0$, this does not necessarily mean the step is impossible since the de Donder relations apply to $cal(A)_i$ and not $A^std_i$.

We know that
$ cal(A)_i = cal(A)^std_i - R T ln(product_j a_(j)^(nu_(i,j))), quad cal(A)^std_i = R T ln(K_"a "),  $
such that
$ cal(A)_i = R T ln(K_"a "/(product_j a_(j)^(nu_(i,j)))) = R T ln(((product_j a_(j)^(nu_(i,j)))_"eq")/(product_j a_(j)^(nu_(i,j)))). $ <eq:affinity>
Therefore, the viability of the reaction mechanism at a given set of reaction conditions is dependent on how much the species activities (or concentrations, in the ideal case) differ from those at equilibrium.
For instance, consider the moment the reaction is started.
We will have only #ce("H2") at the start and therefore $cal(A)_1 > 0$ since $a_ce("H")approx 0$ and $ln(y\/x)-> infinity$ for $x->0$.
This makes sense because the only direction the reaction can initially proceed is in the forward direction.
When the extent of reaction is non-negligible (e.g. at steady state), however, the highly negative value of $cal(A)_1$ suggests the step would be unlikely to proceed in the forward direction.

Ultimately, this brings us to the idea of kinetic coupling wherein a thermodynamically unfavorable (i.e. $cal(A)^std_i<0$) step can be overcome by having the concentration of a reactant kept high or a product concentration kept low with respect to its standard state, equilibrium value via coupling with another step in the mechanism.
For #ce("H2 <--> 2H^∙") to proceed in the forward direction at steady state, there would likely need to be a separate, favorable reaction that it could kinetically couple to --- namely one that would rapidly and continually consume #ce("H^∙").

=== Thermodynamic and Kinetic Factors in Reversible Reaction Rates

We can now see that there are thermodynamic factors that influence the reversibility of a reaction and, in turn, the net reaction rate.
To make this even clearer, consider the net rate of a reaction step, which we can rewrite in terms of reversibility as
$ r_i = r_(i)^+ - r_(i)^- = r_(i)^+ (1-Z_i) = r_(i)^+ (1-exp(-(cal(A)_i)/(R T))). $

From here, we can make use of #ref(<eq:affinity>) to substitute in for $cal(A)_i$ in our above expression and arrive at
$ r_i =r_(i)^(+) - r_(i)^(-)= r_(i)^+ (1-1/K_"a " product_j a_(j)^(nu_(i,j))). $

We can define $r_(i)^+$ using the Eyring equation (#ref(<eq:eyring_final_rate>)) and arrive at
$ r_i = r_(i)^(+) - r_(i)^(-) =  [(k_"B "T )/h C^std exp(-( Delta G_(i)^std^ddagger) / (R T)) 1/gamma^ddagger product_(j,nu_(i,j)<0) a_(j)^(|nu_(i,j)|)] [1-1/K_"a " product_j a_(j)^(nu_(i,j))]. $
The term on the left-hand side is a significant kinetic term, which is heavily dependent on the activation (free) energy computed at the standard state.
The term on the right-hand side is a significant thermodynamic term, which is heavily dependent on the equilibrium constant also at the standard state and represents the approach to equilibrium.
If the activity of each reactant and product is the same that at equilibrium, the right-hand side becomes 0, and the net rate becomes 0 as well.

= Energy and Reactivity Trends <reaction-energy-diagrams>

== Reaction Energy Diagrams

#self[todo]
// show what it is, references at 0, etc. for a typical catalytic cycle
// show apparent activation energy

== Trends in Adsorption Energies <trends-in-adsorption>

Adsorption is the first step of any catalytic process, so understanding trends in adsorption energies can be particularly valuable for catalyst design purposes. Here, we outline a few common trends in adsorption behavior on surfaces.

=== Physisorption Trends

Since physisorption is governed primarily by van der Waals interactions, it is to be expected that adsorption will generally be stronger for larger adsorbates, particularly those within the same family of molecules.
Adsorption is generally an exothermic process, so stronger adsorption in this case refers to a more negative value.
Generally, the adsorption strength of a molecule can be measured experimentally via temperature-programmed desorption experiments, in which case the desorption energy is the measured quantity and is an endothermic process.
#ref(<fig:physisorption-trends>) shows experimentally measured desorption energies for various $n$-alkanes on three different surfaces.
As the chain length increases, the desorption energy increases (i.e. the adsorbates are more strongly bound).
In this case, the physisorption strength increases linearly with chain length, which makes for a valuable design principle.
It should be noted, however, that trends such as this are going to depend strongly on the adsorbate and the adsorption mode of the adsorbate.
If the adsorbate is bound in an on-top position, for instance, the increased size will likely not result in substantially increased van der Waals interactions.

#figure(
  image("figures/tait_jcp_2006_fig10.png", width: 40%),
  caption: [Desorption energy of $n$-alkanes on various surfaces as a function of chain length. Source: S.L. Tait, Z. Dohnálek, C.T. Campbell, B.D. Kay, _J. Chem. Phys._, 125, 234308 (2006).]
)<fig:physisorption-trends>

=== Chemisorption Trends

Chemisorption is governed by chemical bonding interactions, which are generally more complicated than van der Waals interactions.
Nonetheless, there are countless reports of chemisorption trends in the literature, albeit different than those reported for physisorption.
The classic example is the adsorption of $#ce("CH")_x$ species on the surface of transition metals, which tend to scale linearly with the adsorption energy of a single #ce("C") atom at the same surface site, as shown in #ref(<fig:chx_adsorption>).
The results in #ref(<fig:chx_adsorption>) also highlight how adsorbates in the same family with fewer bonds tend to bind stronger to surfaces.
Importantly, these chemisorption scaling trends are not restricted to hydrocarbons, as demonstrated in #ref(<fig:chemisorption_trends>).

#figure(
  image("figures/norskov_book_fig_6_3.png", width:50%),
  caption: [Adsorption energy for various $#ce("CH")_x$ adsorbates as a function of the C atom adsorption energy for two different surface sites across various transition metals. Source: Figure 6.3 in J.K. Nørskov, F. Studt, F. Abild-Pedersen, T. Bligaard, _Fundamental Concepts in Heterogeneous Catalysis_. ]
) <fig:chx_adsorption>


#figure(
  image("figures/norskov_book_fig_6_6.png", width:75%),
  caption: [Chemisorption trends for various species across transition metal and metal sulfide surfaces. Source: Figure 6.6 in J.K. Nørskov, F. Studt, F. Abild-Pedersen, T. Bligaard, _Fundamental Concepts in Heterogeneous Catalysis_. ]
) <fig:chemisorption_trends>


== Trends in Rates <trends-in-rates>

In this subsection, we highlight different ways to estimate trends in reactivity.
There are many reasons why one might be interested in this task.
Naturally, being able to predict reactivity trends is essential for learning how to extrapolate to new, yet-to-be-studied reactions before running the necessary experiments.
From a computational perspective, computing activation energies (and, therefore, rates of reaction) can also be incredibly challenging.
In order to engineer new reactions of societal interest, a solid foundation in reactivity trends is essential.

=== Bell--Evans--Polanyi Relationships <bell-evans-polanyi-relationships>

In reaction engineering, the most common type of trend that is invoked is the so-called linear energy relationship, which seeks to linearly correlate $ln(k)$ with some energy difference that is generally easier to measure or is more readily tabulated.
In reaction engineering, the most popular relationship of this type is the Bell--Evans--Polanyi (BEP) relationship:
$ E_"a " = E_0 + alpha Delta H_"rxn", $
where $E_0$ and $alpha$ are constants for a given family of closely related reactions.
Generally, $alpha$ can be thought of as the relative position of the transition state along the reaction coordinate, taken from 0 (towards the reactants, also known as an "early" transition state) to 1 (towards the products, also known as a "late" transition state).

Take the dissociation of the C--H bond across different light alkanes on a catalyst surface.
If this set of reactions follows the BEP relationship, than the bond dissociation enthalpy is linearly correlated with the activation energy, such that stronger C--H bonds (e.g. that of #ce("CH4")) would have a larger activation barrier for cleavage than that of weaker C--H bonds (e.g. that of #ce("C3H8")).

One can rationalize the presence of BEP relationships in part through the Hammond--Leffler postulate, which states that the geometric structure (and, thereby, energy) of a transition state tends to be similar to the reactants than the products when the reaction is exothermic and _vice versa_.

#self[Sketch out the Hammond--Leffler postulate, showcasing early vs. late transition states.]

=== Transition State Scaling Relationships <linear-scaling-relationships>



=== Universality in Scaling Relationships <universiality-in-scaling-relationships>

#self[Highlight scaling relationships in https://doi.org/10.1021/acscatal.8b05178.]

=== Sabatier Principle <sabatier-principle>

=== Volcano Plots <volcano-plots>

== Beyond the Sabatier Limit

// Emphasize the need, because we are restricted to volcano line

=== Breaking Scaling Relationships

=== Decoupling Dissociation and Surface Reactions

// SAAs
// https://pubs.acs.org/doi/10.1021/acs.jpclett.8b01888

=== Spin and Multi-State Reactivity

=== Oscillations and Catalytic Resonance

=== Plasma Activation

// #[https://pubs.acs.org/doi/full/10.1021/acsenergylett.9b00263]


= Complex Reaction Networks

== Conversion, Selectivity, and Yield <conversion-selectivity-and-yield>

As we seen in prior examples, it is possible for multiple products to be produced from a given reaction.
We briefly introduced the idea of conversion already, but it is worth ensuring clarity in vocabulary when describing such reations.
Consider a product P produced from a reactant A in a reaction that may produce multiple products. The conversion of A ($X_"A "$), yield of P ($Y_"P "$), and selectivity of P ($S_"P "$) are defined as:
$
X_"A " &equiv ("moles of A converted") / ("moles of A fed")\
Y_"P " &equiv ("moles of P formed") / ("moles of A fed")\
S_"P " &equiv ("moles of P formed") / ("moles of A converted").
$
Based on these definitions, it should also be noted that
$ Y_"P " = X_"A " S_"P ". $

To make this a bit more concrete, consider the following reaction scheme:
$
ce("A + B &-> P")\
ce("C + B &-> Q").
$

The conversion of A, yield of P, and selectivity towards the production of P are the following, assuming a constant volume reactor:
$
X_"A " &= (conc("A")_0 - conc("A")) / conc("A")_0 = 1 - conc("A") / conc("A")_0\
Y_"P " &= (conc("P") - conc("P")_0) / conc("A")_0\
S_"P " &= (conc("P") - conc("P")_0) / (conc("A")_0 - conc("A")) = (conc("P") - conc("P")_0) / (conc("P") + conc("Q")).
$
where the subscript 0 refers to an initial state.
Strictly, speaking, these definitions are only applicable when $V$ is constant.
Otherwise, the expression must be written directly in terms of moles.

== Delplots <delplots>

#self[make plots]

We must introduce a new concept: the "rank" of a species, which is the numerical order in which a product appears.
If a given product is produced first, it will have rank 1 and is called primary; if produced second, it will have rank 2 and is called secondary, and so on.
Written out, for the reaction #ce("A->B->C->D"), B is primary, C is secondary, and D is tertiary.
This is distinct from the order of that species, which we will reserve for the exponential dependence of the rate on concentration.
In a net reaction composed of many elementary reactions, it is also possible for a species to exhibit multiple ranks.

Plots of species concentrations as a function of time are very powerful ways to determine the rank of different product species.
This becomes immediately apparent when considering the simple reaction
$ ce("A") fwdArrow(k_1) ce("B") fwdArrow(k_2) ce("C"). $
A plot of the species concentrations as a function of time will indicate that B has a non-zero initial slope, whereas C has a zero initial slope since it cannot form until B is first produced.
In mathematical terms, we have
$ (dif conc("A"))/(dif t) = - k_1 conc("A"), quad quad (dif conc("B"))/(dif t) = k_1 conc("A") - k_2 conc("B"), quad quad (dif ce("C"))/(dif t) = -k_2 conc("B"). $
For $t->0$, we have $conc("A") ->conc("A")_0 $, $conc("B")->0$, and $conc("C") -> 0$ such that 
$ ((dif conc("A"))/(dif t))_(t->0) = - k_1 conc("A")_0, quad quad ((dif conc("B"))/(dif t))_(t->0) = k_1 conc("A")_0, quad quad ((dif conc("C"))/(dif t))_(t->0) = 0. $

This is in contrast with a parallel reaction scheme like
$
ce("A") &fwdArrow(k_1) ce("B")\
ce("A") &fwdArrow(k_2) ce("C"),
$
where both B and C would have a non-zero initial slope unlike the series reactions.
Again, in mathematical form we can write
$  (dif conc("A"))/(dif t) = -k_1 conc("A") - k_2 conc("A"), quad quad (dif conc("B"))/(dif t) = k_1 conc("A"), quad quad (dif conc("C"))/(dif t) = k_2 conc("A"). $
For $t->0$, we have
$  ((dif conc("A"))/(dif t))_(t->0) = - (k_1 + k_2)conc("A")_0, quad ((dif conc("B"))/(dif t))_(t->0) = k_1 conc("A")_0, quad ((dif conc("C"))/(dif t))_(t->0) = k_2 conc("A")_0. $
This kind of analysis can be extended to higher ranks and even to reactions of various orders to discern the rank of different species in a reaction network.

This brings us to the concept of a delplot, which is a graphical method to determine the rank of a reaction product.
#footnote[N.A. Bhore, M.T. Klein, K.B. Bischoff, The Delplot Technique: A New Method for Reaction Pathway Analysis, _Industrial and Engineering Chemistry Research_, 29, 313--316 (1990).]
A delplot is a plot of $Y_"P " \/ X_"A "^m$ where $m$ is referred to as the rank of the delplot.
By investigating the behavior at $X_"A "->0$ (i.e. $t->0$), we can identify the rank of the species.

This first-rank delplot is used to distinguish primary products from non-primary products, while higher rank delplots allow for the discernment of products that are secondary, tertiary, and so on.
The following notation is used to describe delplots: $#delplot(1)$.
Here, the left-hand superscript is a number representing the rank of the delplot (in this case $m=1$), P represents the product species under investigation, and the subscript A means that the delplot is based on the conversion of species A.

The first-rank delplot is simply a plot of selectivity versus conversion.
In other words, it is a plot of $Y_"P " \/ X_"A "$ vs. $X_"A "$.
For each product P, the delplot is extrapolated to $X_"A " -> 0$, typically corresponding to the initial time point for the reaction.
This $y$-intercept, denoted as #delplot(1), can be evaluated as

$ #delplot(1) = lim_(X_"A "->0)(Y_"P "  / X_"A ") = lim_(X_"A "->0)((conc("P") \/ conc("A")_0) / (1 - conc("A") \/ conc("A")_0)) = lim_(X_"A "->0)(conc("P") / (conc("A")_0 - conc("A"))) $<eq:delplot1>

#ref(<eq:delplot1>) can be evaluated algebraically from known expressions of #conc("P") and #conc("A") as a function of time or numerically from the experimentally determined plots of selectivity versus conversion.
Since the rate expressions are often not known _a priori_, delplots are typically a graphical approach based on experimental data.
As previously alluded to, if a finite $y$-intercept is found in the first-rank delplot for species P, then P is primary since it is immediately produced upon the consumption of A.
If the $y$-intercept is zero, then P is a non-primary product (i.e. the rank is greater than 1) because it lags behind the formation of the previous intermediate in the reaction network.

Higher rank delplots allow for the sorting of products of rank greater than 1.
The second-rank delplot consists of a plot of $Y_"P " \/ X_"A "^2$ vs. $X_"A "$.
The $y$-intercept of the second-rank delplot for a product P is
$ #delplot(2) = lim_(X_"A "->0)(Y_"P "  / X_"A "^2) = (conc("P") \/ conc("A")_0) / (1-conc("A")\/conc("A")_0)^2 = (conc("A")_0 conc("P")) / (conc("A")_0 - conc("A"))^2. $
If a finite intercept is found, the product is secondary.
If a zero intercept is found, the product's rank is greater than secondary.
If a divergence is found as $X_"A "->0$ (i.e. no $y$-intercept), the product's rank is primary.

This can be summarized more generally.
For a delplot of rank $m$ presented as a plot of $Y_"P " \/ X_"A "^m$ vs. $X_"A "$, the $y$-intercept can be found via
$ " "^(m)"P "_"A " = lim_(X_"A "->0) (Y_"P " / (X_"A "^m)) = lim_(X_"A "->0)((conc("P") \/ conc("A")_0) / (1-conc("A")\/conc("A")_0)^m). $
The general procedure presented in #ref(<table:delplot>) can then be employed, which we have extended to different reaction orders as well.
#footnote[While a useful tool, delplots can be of limited use when $m$ is large due to propagation of error associated with raising low-conversion limit data to a large power.]
#figure(
  table(
    columns: 3,
    table.header(
      [Order],
      [$"Rank"=1$],
      [$"Rank">1$]
    ),
    [$1$], [#delplot(1) = finite], [$""^(m)"P "_"A " = 0$ for $"rank">m$\ $""^(m)"P "_"A " = "finite"$ for $"rank"=m$\ $""^(m)"P "_"A " = "diverges"$ for $"rank"<m$],
    [$<1$], [#delplot(1) = finite], [$""^(m)"P "_"A " = 0$ for $"rank">m$\ $""^(m)"P "_"A " = "diverges"$ for $"rank"=m$],
    [$>1$], [#delplot(1) = finite], [$""^(m)"P "_"A " = 0$ for $"rank">=m$\ $""^(m)"P "_"A " = "finite"$ for $"rank"<m$],

  ),
  caption:[Summary of delplot intercepts for a product P formed from the conversion of A. $m$ is the rank of the delplot, whereas "rank" refers to the rank of product P.]
)  <table:delplot>



== Degree of Rate Control <degree-of-rate-control>

=== The Degree of Rate Control of an Elementary Step

In #ref(<rate-determining-step>), we introduced the concept of a rate-determining step that can greatly simplify a proposed mechanism.
The rate-determining step also indicates, albeit indirectly, where improvements can be made to tune the overall rate of reaction most effectively.
While a valuable principle, in most cases, it is not possible to identify a single rate-determining step.
In these scenarios, we can rely on a concept known as the degree of rate control to guide our reaction engineering efforts.
#footnote[For further details, refer to C.T. Campbell, The Degree of Rate Control: A Powerful Tool for Catalysis Research, _ACS Catalysis_, 4, 2770--2779 (2017). https://doi.org/10.1021/acscatal.7b00115.]

The degree of rate control, $X_("RC",i)$ for elementary step $i$ is defined as
$ X_("RC",i) equiv k_i / r ((diff r) / (diff k_i))_(k_(j != i), K_i) = 1/r ((diff r) / (diff ln(k_i)))_(k_(j != i), K_i). $ <eq:drc>
Here, the partial derivative is taken such that the forward and reverse rate constants, $k_j$, for all steps other than $i$ are constant.
Additionally, the equilibrium constant, $K_i$, for step $i$ is held fixed.
While not explicitly written, it is also tacitly assumed that the reaction conditions themselves are constant (e.g. $T$, species concentrations) since we are focusing on the impact that an infinitesimal change on only the rate constant has on the rate.
The reason that there is a factor of $k_i\/r$ proceeding the partial derivative in #ref(<eq:drc>) is to ensure that the expression is normalized such that it describes the relative change in the net rate scaled by the given $k_i$.

By definition, if $X_("RC",i)$ is positive, then it implies that increasing the rate constant $k_i$ will increase the reaction rate and _vice versa_.
In this case, step $i$ is said to be a rate-limiting (rather than determining) step.
Similarly, if $X_("RC",i)$ is negative, it implies that increasing the rate constant $k_i$ will decrease the reaction rate and _vice versa_.
In this case, step $i$ is said to be a rate-inhibiting step.

Since the degree of rate control is most often applied to understand how perturbations on a free energy diagram influence reactivity, it is natural to replace $k_i$ in #ref(<eq:drc>) with free energy.
From the Eyring equation, we know that
$
k_i &= C^std^(1-n) (k_"B "T )/h exp(-(Delta G_(i)^std^ddagger) / (R T))\
ln(k_i) &= ln(C^std^(1-n) (k_"B "T )/h) - (Delta G_(i)^std^ddagger) / (R T).
$
Taking the derivative of both sides under the assumption that we are considering a given value of $T$ yields
$ diff ln(k_i) = - diff((Delta G_(i)^std^ddagger) / (R T)) $
since the derivative of a constant is always zero.
Therefore, we can write that the degree of rate control is also equal to
$
X_("RC",i) = 1/r ((diff r) / (diff (-(Delta G_(i)^std^ddagger) / (R T))))_(k_(j != i),K_i)
= 1/r ((diff r) / (diff (-(G_("TS",i)^std) / (R T))))_(G_(j!=i)^std^dagger,G_(m)^std),
$
where the partial derivative in the latter expression is taken such that the Gibbs free energy of the transition states for all steps other than $i$ are constant and the Gibbs free energy of all intermediates $m$ are constant.

=== The Thermodynamic Degree of Rate Control for an Intermediate

Admittedly, thinking about modifying the free energy of a transition state is a bit awkward.
It is far more intuitive to think about how one might change the free energy of an intermediate along the reaction pathway.
Thankfully, it is straightforward to extend the degree of rate control concept to intermediates as well, in which case it is often referred to as the thermodynamic degree of rate control.
The thermodynamic degree of rate control for intermediate $n$ is given as
$ X_("TRC",n) equiv 1/r ((diff r)/ (diff (-G_(n)^std / (R T))))_(G_(m != n)^std, G_(i)^std^ddagger). $
Here, we are taking the derivative with the Gibbs free energy of all reactants, products, and intermediates other than $n$ as constant as well as the free energy of all transition states as fixed.
The difference between $X_("RC",i)$ and $X_("TRC",n)$ is graphically depicted in #ref(<fig:drc>).
#figure(
  image("figures/campbell_acs_catal_2017_fig1.gif", width: 50%),
  caption: [ "Schematic standard-state free-energy surface for the reaction AC(g) + B(g) → AB(g) + C(g), showing the degree of rate control based on a transition state free energy change and intermediate free energy change. Source: C. Campbell, _ACS Catal._, 4, 2770--2779 (2017). ]
)<fig:drc>

=== The Generalized Degree of Rate Control

Given the similarity between $X_("RC",i)$ and $X_("TRC",n)$, a generalized degree of rate control for species $i$ can be written as
$ "DRC"_i = 1/r ((diff r) / (diff (-G_(i)^std / (R T))))_(G_(j != i)^std) = - ((diff ln(r)) / (diff ((G_i^std) / (R T))))_G_(j!=i)^std, $<eq:drc_general>
where the partial derivative is now taken such that the free energy of all species other than $i$ (including reactants, products, intermediates, transition states) is held fixed.
In general,
$ sum_"steps" X_("RC",i) = sum_("TSs") "DRC"_i = 1. $
This implies that for a catalytic mechanism with a single rate-determining step $i$, the value of $X_("RC",i)$ is equal to 1, whereas it is 0 for the remaining steps since they do not contribute to the overall rate to a first approximation. 

Ultimately, this brings us back to the concept of a rate-determining step.
Some argue that there is no such thing as a rate-determining step.
#footnote[S. Kozuch, J.M.L. Martin, The Rate-Determining Step is Dead. Long Live the Rate-Determining State! _ChemPhysChem_, 12, 1413--1418 (2011). https://doi.org/10.1002/cphc.201100137. ]
Instead, what we have are rate-determining states, which are the states (including both minima and transition states) along the reaction coordinate that exhibit the largest degree of rate control.

=== Applying the Degree of Rate Control to a Model Reaction

While the value of the degree of rate control is in being able to analyze complex reaction networks, we will demonstrate its use on a toy reaction scheme simply for demonstration purposes.
Consider the following reaction:
$ 
ce("A + *") &eqArrow(k_1,opposite:k_(-1)) ce("A^*")\
ce("B + *") &eqArrow(k_2,opposite:k_(-2)) ce("B^*")\
ce("A^* + B^*") &fwdArrow(k_3) ce("AB^* + *") quad ("RDS")\
ce("AB^*") &eqArrow(k_4,opposite:k_(-4)) ce("AB + *")
$
for the surface-catalyzed reaction #ce("A + B -> AB") where the third reaction is the rate-determining step, thereby making the other steps in quasi-equilibrium.
For the sake of demonstration, assume that the rate law can be approximated by 
$ r = k_3 K_2/K_1 p_"B "/p_"A ". $

We can write the rate law in terms of standard-state Gibbs free energies using the Eyring equation for $k_3$ and the definition of the equilibrium constant for $K_1$ and $K_2$:
$
r &= [(k_"B " T)/h 1/C^std exp(-(Delta G_(3)^std^ddagger)/(R T))] [(C^std exp(-(Delta G_(2)^std)/ (R T)))/(C^std exp(-(Delta G_(1)^std)/ (R T)))] p_"B "/p_"A "\
r &= (k_"B " T)/h p_"B "/p_"A " 1/C^std exp((-Delta G_(3)^std^ddagger - Delta G_(2)^std + Delta G_(1)^std)/(R T)) \
r &= (k_"B " T)/h p_"B "/p_"A " 1/C^std exp((-[G_(3)^std^ddagger - G_ce("A^*")^std - G_ce("B^*")^std] - [G_ce("B^*")^std - G_ce("B")^std - G_ce("*")^std] + [G_ce("A^*")^std - G_ce("A")^std - G_ce("*")^std])/(R T)) \
r &= (k_"B " T)/h p_"B "/p_"A " 1/C^std exp((-G_(3)^std^ddagger + 2 G_ce("A^*")^std + G_ce("B")^std - G_ce("A")^std)/(R T))\
r &= (k_"B " T)/h p_"B "/p_"A " 1/C^std exp((-G_(3)^std^ddagger + 2 G_ce("A^*")^std)/(R T)),
$
where the reactants (A and B) are taken as the zero-energy reference and can therefore be removed.
Now we can calculate the generalized degree of rate control using #ref(<eq:drc_general>) to arrive at
$ "DRC"_"TS"_3 = 1 $
$ "DRC"_ce("A^*") = -2 $
and $ "DRC"_j = 0$ for all other species.
The degree of rate control for the rate-determining state (i.e. the transition state associated with the rate-determining step) is 1, which is a general feature of rate-determining steps.

From this analysis, we can gain insights into how to improve the reaction via catalyst design.
In general, we wish to stabilize transition states with large, positive degrees of rate control since it is these species that have the most outsized impact on the rate.
However, we need to do so without significantly stabilizing intermediates with large, negative degrees of rate control since this would counteract the improvements to the rate. 

= Approaches and Tools for Reaction and Reactor Modeling

#self[Todo... maybe also focus on literature readings on interesting articles? SAAs? Scaling relationships? Let the class vote for what they want!]

We will provide a high-level overview of computational reaction engineering in this section. 
While this course is not a computational reaction engineering course, it is still valuable to understand what methods are out there and how they can be used to advance the field of reaction engineering.
This section also highlights how all of the topics in this course can come together in a synergistic way.

// == Density Functional Theory

// #footnote[If you are interested in learning more about DFT, check out the book _Density Functional Theory: A Practical Introduction_ by D. Sholl and J.A. Steckel.]

// == Microkinetics <microkinetic-modeling>

// #footnote[A.H. Motagamwala, J.A. Dumesic, Microkinetic Modeling: A Tool for Rational Catalyst Design, _Chemical Reviews_, 121, 1049--1076 (2021). https://doi.org/10.1021/acs.chemrev.0c00394.]

// // background needed. why are we doing this?
// // what do we seek to predict?
// // comparison against experiments and value?

// In general, microkinetic modeling involves the following steps:

// 1. Constructing a set of elementary reactions
// 2. Deriving rate expressions for each elementary step, including estimates, predictions, or experimental measurements of rate constants
// 3. Constructing a system of ordinary differential equations (ODEs) to describe the rates of change of each species
// 4. Defining initial values for species concentrations (i.e. at $t=0$), model parameters (e.g. $T$), and partial pressures of gas-phase species
// 5. Solving the system of ODEs to product concentration profiles as a function of time
// 6. Comparison against experiments and/or prior theories

// === Rate Expressions

// In #ref(<analytical-rate-expressions>) and #ref(<rate-expressions-for-heterogeneous-reactions>), we derived analytical rate expressions for homogeneous and heterogeneous reactions like for the relatively simple LHHW kinetic models in #ref(<langmuir-hinshelwood-hougen-watson-kinetics>).
// While undoubtedly valuable, most complex reaction networks are not amenable to such algebraic approach.
// In this section, we will take a more numerical approach known as microkinetic modeling, wherein we seek to solve for species concentrations as a function of time.
// We will primarily focus on microkinetic modeling of reactions in heterogeneous catalysis, but the approach is fully extensible to gas-phase or liquid-phase reactions by neglecting all discussions about coverages.

// We start by writing the generalized form of rate of an elementary reaction $i$, originally described in #ref(<eq:general_math_irreversible_rxn>) and now rephrased to be based on fractional coverages for surface species and partial pressures for gas-phase species:
// $ r_i =  k_(i)^+ product_(j,nu_(i,j)<0) p_(j)^(|nu_(i,j)|) theta_(j)^(|nu_(i,j)|). $
// Here, we are implicitly defining $r_i$ to be in units of 1/time (i.e. a TOF).
// If $theta_j$ were surface species concentrations, then $r_i$ would have the typical units for the rate (e.g. M/s).

// With the above expression for the net rate of a single elementary reaction $i$, we can write a system of non-linear ordinary differential equations representing the change in coverage of each adsorbed species $j$ as
// $ (dif theta_j)/(dif t) = sum_i nu_(i,j) r_i, $<eq:theta_evolve>
// which is simply a slight restatement of #ref(<eq:sum_stoichs_rate>).
// #footnote[This results in a challenging but still tractable set of differential equations to solve. Recent work has shown that reformulating these equations in terms of the absolute number of sites, rather than coverages, can be computationally advantageous: S. Vijay, H.H. Heenen, A.R. Singh, K. Chan, J. Voss, _Journal of Computational Chemistry_, 45, 546--551 (2023).]
// Combining these two expressions yields a system of ODEs given by
// $ (diff theta_j)/(diff t) = sum_i (nu_(i,j) k_(i) product_j p_(j)^(|nu_(i,j)|) theta_(j)^(|nu_(i,j)|)). $
// Here, $diff theta_j\/diff t$ should be constructed for each surface species as well as the vacant sites themselves (i.e. $diff theta_*\/diff t$), which is treated as a pseudo-species.
// This may look mathematically complex, but it is really not terribly difficult.

// Consider the toy reaction mechanism
// $
// ce("A + *") &eqArrow(k_1,opposite:k_(-1)) ce("A")^*\
// ce("B + *") &eqArrow(k_2,opposite:k_(-2)) ce("B")^*\
// ce("A")^* &eqArrow(k_3,opposite:k_(-3)) ce("B")^*.
// $

// The system of ODEs would be given by
// $ 
// (diff theta_"A ")/(diff t) &= k_(1) p_"A " theta_* - k_(-1) theta_"A " - k_3 theta_"A " + k_(-3) theta_"B "\
// (diff theta_"B ")/(diff t) &= k_(2) p_"B " theta_* - k_(-2) theta_"B " + k_3 theta_"A " - k_(-3) theta_"B "\
// (diff theta_"* ")/(diff t) &= - k_(1) p_"A " theta_* + k_(-1) theta_"A " - k_(2) p_"B " theta_* + k_(-2) theta_"B ".
// $

// There are also two additional physical constraints that we should ensure are satisfied, namely that each surface coverage $theta_j$ is between 0--1 and that the conservation of mass is conserved: $1 = theta_* + sum_j theta_j$.
// #footnote[We have implicitly assumed that each adsorbate binds to one adsorption site, as is the case for a mondentate adsorption mode. Slight modifications are needed to account for multi-dentate adsorption.]
// We would solve this system of ODEs with a given set of initial conditions (say, $theta_"A "=theta_"B "=0$ and $theta_*=1$), a given temperature (e.g. $T= 600$ K), and a given set of partial pressures (e.g. $p_"A " = 10$ bar, $p_"B "= 1$ bar).
// The simulation would be carried out until steady-state is reached.

// The rate constants for each reaction are generally obtained from experiments, estimates, and/or quantum-chemical calculations depending on the available data and the expected importance of that reaction step.
// For clarity, we will reiterate the Eyring equation from #ref(<eq:eyring-final>) in defining the rate constant for the $i$-th reaction in a mechanism:
// $ k_(i) = (k_"B "T )/h C^std^(1-m) exp(( Delta S_(i)^std^ddagger) / R) exp(-(Delta H_(i)^std^ddagger) / (R T)). $

// While the Eyring equation can also be used to describe both the forward and reverse rate constants for a reversible reaction, we will instead take advantage of the fact that this can be determined from the equilibrium constant to ensure internal consistency.
// The equilibrium constant for the $i$-th elementary reaction is defined based on the reaction free energies, obtained from tabulated experimental data and/or quantum-chemical calculations via
// #footnote[It is critical to ensure that thermodynamic consistency is maintained, such that the individual reaction enthalpies and entropies appropriately sum up to those of the net reaction. For details on this matter, refer to A.B. Mhadeshwar, H. Wang, D.G. Vlachos, _Journal of Physics B_, 107, 12721--12733 (2003).]
// $ K_("eq",i) = exp(-(Delta G_(i)^std)/(R T)). $
// The equilibrium constant is then used to find the reverse rate constant via
// $ k_(i)^(-) = k_(i)^(+)/K_("eq",i). $

// === Demonstration

// There are several codes available for microkinetic modeling.
// Here, we will provide a brief, high-level demonstration of MKMCXX (https://www.mkmcxx.nl) to showcase how  
// #self[https://wiki.mkmcxx.nl/index.php/My_first_simulation]

// == Reactor Modeling <chemical-reactor-modeling>

// In the previous subsection, we discussed microkinetic modeling and how it can be used to predict and understand concentration profiles as a function of time.
// The natural extension to this type of model is to ask how one might model a chemical reactor, including the coupling of kinetics, thermodynamics, and transport processes as a function of time.
// In this subsection, we will do just that using the open-source software package Cantera, which can be found at https://cantera.org.
// #footnote[If you are interested in learning more about the fundamentals of how to simulate chemically reacting systems, check out the following comprehensive textbook: R.J. Kee, M.E. Coltrin, P. Glarborg, H. Zhu, _Chemically Reacting Flow: Theory, Modeling, and Simulation_.]

// === Automated Reaction Network Generation

// Naturally, to have an accurate model, one must start with a reaction network containing all potentially significant reactions for the system under investigation.
// In addition the kinetic parameters used in the microkinetic model, one also needs thermochemical properties for each species so that the energy balance can be solved.
// The NIST Chemistry WebBook (https://webbook.nist.gov/chemistry) is an excellent source of experimental thermochemistry data.

// Naturally, the question arises --- how does one know what the potentially significant reactions might be?
// While intuition and prior experiments are certainly a solid start, recent advances in computational chemistry have ushered in an age of automated reaction mechanism generation.
// One particularly notable example is the Reaction Mechanism Generator (RMG) (https://rmg.mit.edu) by Bill Green and Richard West.
// #footnote[C.W. Gao, J.W. Allen, W.H. Green, R.H. West, "Reaction Mechanism Generator: Automatic construction of chemical kinetic mechanisms", _Comput. Phys. Commun._, 203, 212--225 (2016).]
// With RMG, a library of reaction rules are established that define common chemical transformations vi 

// Some of the species generated by a reaction network generator like RMG may not have tabulated thermochemical properties, either because they are too reactive to be isolated or because they simply have not been investigated yet.
// In this case, thermochemical properties can be estimated from Benson group addivity methods or computed from quantum-chemical methods like DFT.

// Kinetics again estimated or computed from DFT.

// === Simulating the Reactor

= Concluding Thoughts <outlook>

#footnote[P. Bollini et al., Vision 2050: Reaction Engineering Roadmap, _ACS Engineering Au_, 6, 364--390 (2023). https://doi.org/10.1021/acsengineeringau.3c00023.]
