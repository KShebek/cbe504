#import "@preview/xarrow:0.3.1": xarrow
#import "@preview/gentle-clues:0.9.0": tip, clue, tip
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
#let plot(title: "Interactive Plot", icon: emoji.chart, ..args) = clue(
  accent-color: purple,
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

#set math.equation(numbering: "(1)")

#show: ilm.with(
  title: [Chemical Reaction Engineering],
  author: "Andrew S. Rosen",
  abstract: [Lectures notes for a graduate-level course.],
  paper-size: "us-letter",
  preface: [#align(center + horizon)[Copyright #sym.copyright 2024 Andrew S. Rosen.

Licensed under the Creative Commons Attribution-NonCommercial-ShareAlike 4.0 License (the "License"). You may not use this file except in compliance with the License. You may obtain a copy of the License at https://creativecommons.org/licenses/by-nc-sa/4.0.

A.S.R gratefully acknowledges Prof. Aditya Bhan, Prof. Linda Broadbelt, and Prof. Justin Notestein for inspiration on various topics covered within this course.

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
One of many examples of this is in the proper treatment of lattice statistics in describing multi-site adsorption and surface reaction phenomena.

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

#caution[When we refer to a standard state, this is a choice that the practicioner makes. The standard state thermodynamic properties are independent of the pressure at which the reaction is actually carried out; rather, they are associated with a hypothetical process.
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
To learn more about potential pitfalls when neglecting activity in equilibrium expressions, refer to C.G. McCarty, E. Vitz, pH Paradoxes: Demonstrating That It Is Not True That pH ≡ -log[H+], _J. Chem. Educ._, 83, 752--757 (2006).]



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

=== The van~'t Hoff Equation

// Note to self: The Arrhenius aside was more problematic than not. Should probably avoid going forward.

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
#footnote[Recall that $Delta H^std$, which is the enthalpy change associated with the reaction, can be calculated from the tabulated standard state enthalpy of formation, $Delta H_("f ",j)^std$, for each species via $Delta H^std = sum_j nu_j Delta H_("f ",j)^std$. The enthalpies of formation of many species are tabulated in thermochemical handbooks.]

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

// cut this whole section..

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

// NOTE TO SELF: This one did not go well in class. A bit too abstract without a concrete example. Probably best to avoid altogether in the future.

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
We have $[A]$ increasing with $t$ without bound despite being the reactant.
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
From here, we could once again substitute and integrate to find $conc("C")$.
That said, it is a fair bit simpler to just invoke the conservation of mass:
$ conc("C") = conc("A")_0 - conc("A") -conc("B"). $

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
  - If we say that $R_2=0.31$, then we would ask: is $a_1 > R_1 dot a_"tot"$? The answer is yes for our example because $200 > 0.31 dot 200$, so we take $j=1$ and state that reaction 1 will occur at our new time point $t_1$. #footnote[You might notice that it will always be the case that $j=1$ is selected by this algorithm in the first time step. This is to be expected because #ce("B->C") cannot occur unless #ce("A->B") happens first. When we update the simulation, this will no longer be the case, and the probability that reaction $j=2$ will occur will increase as the population of #ce("B") increases.] 
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
$ ce("A") fwdArrow(k_1) ce("B") fwdArrow(k_2) ce("C"), quad r_2>>r_1. $

If we were to invoke PSSH, we would state $r_ce("B") approx 0$.
Note that it does _not_ mean that we directly set #conc("B") to zero or a constant value, which would have the potential to over-simplify the equations, as we will justify below.

To demonstrate the PSSH, we will revisit the solutions to the differential equations that were derived for the #ce("A->B->C") series reaction in #ref(<coupled-reactions>):
$ conc("A") = conc("A")_0 e^(-k_1 t). $
$ conc("B") = conc("A")_0 (k_1 ) / (k_2 - k_1) (e^(-k_1 t) - e^(-k_2 t)). $
$ conc("C") = conc("A")_0 - conc("A") -conc("B"). $

If $r_2>>r_1$ and we treat $conc("B")$ as small (e.g. in the case of B being a highly reactive intermediate), then we can conclude $k_2>>k_1$ and we can approximate the above expressions as follows:
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
One might propose based on the stoichiometry a rate law of the form
$ r =^? k conc("NO")^2 conc("O2"). $<eq:no2_rate_law>
It is known from experiments that the rate of production of the product, $r_ce("NO2")$, is second-order in #ce("NO") and first-order in #ce("O2").
However, termolecular reactions are extremely rare.
Even stranger, $r_ce("NO2")$ is found to _decrease_ with increasing temperature.
These details allow us to conclude that #ref(<eq:no2_rxn>) cannot actually be an elementary reaction. 
We will explain the so-called "anti-Arrhenius" behavior below.

First, we must propose a mechanism.
Since it is not expected that you know the intricacies of atmospheric chemistry, we will simply take the following as provided to us:
$ ce("NO + O2") eqArrow(k_1, opposite: k_(-1)) ce("NO3^∙") $
$ ce("NO3^∙ + NO") fwdArrow(k_2) ce("2 NO2") $<eq:no2_slow_step>
Then we write out the true elementary rate law for $r_ce("NO2")$:
$ r_ce("NO2") = 2k_2 [ce("NO3^∙")] [ce("NO")]. $<eq:rate_no2>
Note the factor of 2 in #ref(<eq:rate_no2>), which is needed because two #ce("NO2") molecules are produced for every reaction of #ce("NO3^∙") and #ce("NO"), as originally noted in #ref(<eq:stoichs>).

At this point, we can invoke the PSSH to simplify matters.
Namely, the short-lived intermediate #ce("NO3^∙") will have a net rate of formation that is essentially zero:
$ r_ce("NO3^∙") approx 0 = k_1 conc("NO") conc("O2") - k_(-1) conc("NO3^∙") - k_2 conc("NO3^∙") conc("NO"). $<eq:rate_no3_rad>
Ideally, we would like to substitute in for #ce("NO3^∙") in #ref(<eq:rate_no2>) since it is not easy to observe experimentally given its short lifetime.
With this in mind, we will solve for #ce("NO3^∙") in #ref(<eq:rate_no3_rad>) to get
$ conc("NO3^∙") = (k_1 conc("NO") conc("O2")) / (k_(-1) + k_2 conc("NO")). $<eq:rate_no3_rad_rearrange>
Substituting in #ref(<eq:rate_no3_rad_rearrange>) into #ref(<eq:rate_no2>) yields
$ r_ce("NO2") = (2 k_1 k_2 conc("NO")^2 conc("O2")) / (k_(-1) + k_2 conc("NO")). $<eq:rate_no2_rearrange>

There is some particularly unusual behavior for this reaction that is worth taking a further look at.
It is known that  #ref(<eq:no2_slow_step>) is relatively slow compared to the reverse of #ref(<eq:no2_rxn>), such that we can state $k_2 conc("NO")<<k_(-1)$ and we can simplify #ref(<eq:rate_no2_rearrange>) to the following:
$ r_ce("NO2") = (2 k_1 k_2) / (k_(-1)) conc("NO")^2 conc("O2"). $<eq:r_no2_k>
which can be rewritten as
$ r_ce("NO2") = 2 (A_1 A_2) / (A_(-1)) exp(-(E_"a,1" - E_("a,"-1) + E_"a,2") / (R T)) conc("NO")^2 conc("O2"). $<eq:no2_rate_law_real>
If we define
$ A_"app" equiv (A_1 A_2) / A_(-1) $
$ E_"a,app" equiv E_"a,1" - E_("a,"-1) + E_"a,2", $
where $A_"app"$ and $E_"a,app"$ are an apparent pre-factor and activation barrier,
#footnote[Note that applying the definition of the apparent activation energy (#ref(<eq:apparent_e_a>)) to $k_"app" equiv k_1 k_2 \/ k_(-1)$ would yield the same expression for $E_"a,app"$.]
then
$ r_ce("NO2") = 2 A_"app" exp(-E_"a,app" / (R T)) conc("NO")^2 conc("O2"). $
From here, the rate of reaction, $r$, can be computed simply as $r_ce("NO2")\/2$.

If $E_"a,app" < 0$, then the reaction can have anti-Arrhenius behavior where the rate _decreases_ with increasing temperature.
Indeed, this reaction is known from experiments to have an empirically measured kinetic barrier of --3.3 kJ/mol.
This example is also a good demonstration of the fact that one cannot definitively prove a mechanism is accurate, as both #ref(<eq:no2_rate_law>) and #ref(<eq:r_no2_k>) have the same functional form, but the former implies an elementary reaction, whereas the latter implies that there exist multiple steps.
