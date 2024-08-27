#import "@preview/dvdtyp:1.0.0": dvdtyp, example
#import "@preview/xarrow:0.3.1": xarrow
#import "@preview/gentle-clues:0.8.0": info, tip, clue
#import "@preview/whalogen:0.2.0": ce

#let read(title: "Additional reading", icon: emoji.book , ..args) = clue(
  accent-color: silver,
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
#show: dvdtyp.with(
  title: "Chemical Reaction Engineering",
  subtitle: [Lecture Notes for a Graduate-Level Course],
  author: "Andrew S. Rosen",
  
)

= Transition State Theory <transition-state-theory>

#read["Section 2.3. Transition-State Theory of Reaction Rates" in J.B. Butt, _Reaction Kinetics and Reactor Design_ (2#super("nd") ed.).]

In the previous sections, we have made extensive use of the rate coefficient, $k$.
Here, we seek to provide theoretical insights into what factors dictate the value of $k$ while also providing an atomistic justification for the functional form of the Arrhenius equation.

// More context needed

== Setting the Stage <setting-the-stage>

=== Rate in Terms of an Equilibrium Constant

We seek to write an expression for the rate coefficient, $k$, in terms of quantities that can be readily computed from theory and/or experimentally measured.
Transition state theory (TST) is what makes this possible.
We will start by considering the following reaction:
#footnote[We have chosen a bimolecular reaction here simply for demonstration purposes, but the results that follow are not inherently restricted to a particular molecularity if proper care is taken.]
$ ce("A + B") &fwdArrow(k) ce("P") $
In transition state theory, we assume that there is some transient complex (the transition state) that connects the reactants and products along the minimum energy pathway of the reaction coordinate.
This means we can rewrite our reaction as
$ ce("A + B") &eqArrow(k_(1)^*,opposite:k_(-1)^*) ce("AB")^ddagger fwdArrow(k_2^*) ce("P"). $<eq:tst_rxn>

The rate of product formation can similarly be written from both the macroscopic and microscopic perspectives as
$
r &= k conc("A") conc("B")\
r &= k_(2)^* conc("AB")^ddagger.
$
By equating these two expressions and rearranging, we have
$ k = k_(2)^*  (conc("AB")^ddagger)/(conc("A") conc("B")). $
There is clearly more work to do here.
For one, we have a $conc("AB")^ddagger$ term, which is not a measurable quantity given the transient nature of the transition state.
We will take care of this challenge below, after which we will address the somewhat nebulous $k_(2)^*$ term.

Let us consider the rate of formation of the transition state, which can be written in the usual way as
$ r_(ce("AB")^ddagger) = k_(1)^* conc("A") conc("B") - k_(-1)^* conc("AB")^ddagger - k_(2)^* conc("AB")^ddagger. $
Since there is inherently some activation barrier for this process, we can comfortably state that $#ce("A + B") -> ce("AB")^ddagger$ is slow compared to both $#ce("AB")^ddagger -> ce("A + B")$ and $ce("AB")^ddagger -> ce("P")$.
In other words, we can invoke the pseudo-steady state hypothesis given the short-lived nature of the transition state.
#footnote[
In most traditional treatments of transition state theory, it is instead assumed that there is a quasi-equilibrium between the reactants and the transition state, implying that $k_(1)^* = k_(-1)^*$ and resulting in a slightly different derivation.
However, this is a poor assumption because there is only an energetic barrier between the reactants and transition state, not the reverse.
The interested reader can refer to the following article for more details: J.F. Perez-Benito, "Some Considerations on the Fundamentals of Chemical Kinetics: Steady State, Quasi-Equilibrium, and Transition State Theory," _J. Chem. Ed._, 94, 1238--1246 (2017).
]
In doing so, we arrive at
$
0 &= k_(1)^* conc("A") conc("B") - k_(-1)^* conc("AB")^ddagger - k_(2)^* conc("AB")^ddagger\
0 &= k_(1)^* conc("A") conc("B") - conc("AB")^ddagger (k_(-1)^* + k_(2)^*).
$
After a tiny bit of algebraic manipulation, we have
$ conc("AB")^ddagger = (k_(1)^* conc("A") conc("B")) / (k_(-1)^* + k_(2)^*). $
For reasons that will be come clearer shortly, we will define $K_"C "^ddagger equiv k_(1)^* \/ k_(-1)^*$ and make the substitution to arrive at
$ conc("AB")^ddagger = (k_(-1)^* K_"C "^ddagger conc("A") conc("B")) / (k_(-1)^* + k_(2)^*). $<eq:TS_conc_TS>
We are left with an expression that contains microscopic rate constants, which is still not particularly convenient since they cannot be readily determined given the transient nature of the transition state.
This can be resolved by introducing a concept known as the transmission coefficient, $kappa$.
We will define $kappa_"f "$ and $kappa_"r "$ as the probability that the transition state will proceed forward to the product(s) or reverse to the reactant(s), respectively.
In other words, we have
$ kappa_"f " + kappa_"r " = 1. $
The rate coefficients of $k_(-1)^*$ and $k_(2)^*$ that remain in our expression for $#conc("AB")^ddagger$ can be written as
$
k_(-1)^* &= kappa_"r " nu^ddagger = (1-kappa_"f ") nu^ddagger\
k_(2)^* &= kappa_"f " nu^ddagger,
$
where $nu^ddagger$ is the frequency (in units of $"time "^(-1)$) associated with the vibrational mode along the reaction coordinate that connects the transition state to the surrounding local minima.
Effectively, the expressions for $k_(-1)^*$ and $k_(2)^*$ can be interpreted as the (probability-weighted) frequency with which the transition state moves back towards the reactants or forward towards the products.
Substituting our expressions for $k_(-1)^*$ and $k_(2)^*$ into #ref(<eq:TS_conc_TS>) yields
$
conc("AB")^ddagger &= ((1-kappa_"f ") nu^ddagger K_"C "^ddagger conc("A") conc("B")) / ((1-kappa_"f ") nu^ddagger + kappa_"f " nu^ddagger)\
conc("AB")^ddagger &= ((1-kappa_"f ") nu^ddagger K_"C "^ddagger conc("A") conc("B")) / (nu^ddagger)\
conc("AB")^ddagger &= (1-kappa_"f ") K_"C "^ddagger conc("A") conc("B").
$
We have arrived now at a relatively nice looking expression for $conc("AB")^ddagger$ that we can substitute into our original expression for $k$ to arrive at
#footnote[The resulting expression for $k$ has a factor of $(1-kappa_"f ")$ that is not included if one assumes a quasi-equilibrium between the reactants and the transition state.]
$
k &= (k_(2)^* (1-kappa_"f ") K_"C "^ddagger conc("A") conc("B")) / (conc("A") conc("B"))\
k &= k_(2)^* (1-kappa_"f ") K_"C "^ddagger\
k &= kappa_"f " (1-kappa_"f ") nu^ddagger K_"C "^ddagger.
$
As a reminder, if we want to know the rate of product generation, then we can say
$ r = k conc("A") conc("B") = kappa_"f " (1-kappa_"f ") nu^ddagger K_"C "^ddagger conc("A") conc("B"). $
It may not look like we have made much progress here in our quest to find a useful expression for $k$, but looks can be deceiving.

=== Rate in Terms of Molecular Partition Functions

The most pressing situation to address in our definition of $k$ is $K_"C "^ddagger$.
Thankfully, with a healthy dose of statistical thermodynamics, this becomes relatively manageable.
Although it will not be derived here, from statistical mechanics it is known that the equilibrium constant can be expressed in terms of molecular partition functions as follows:
$ K_"a " equiv product_i a_(i)^nu_i = product_i Z''_(i)^(nu_i) = product_i Z' exp(- (Delta U^ddagger) / (R T)), $
where $Z''_i$ is the (unitless) molecular partition function of species $i$, $Z'$ is the same unitless molecular partition function but with the ground-state electronic energy of each species factored out so that it becomes a zero-energy reference,
#footnote[The motivation for this decision will become clearer when we discuss the electronic partition function.]
and $Delta U^ddagger$ is the change in internal energy (i.e. sum of electronic energy and zero-point vibrational energy) between the transition state and reactant(s).
We will describe how one determines a molecular partition function in a moment, but before we do, recall that we have been dealing with concentrations and $K_"C "^ddagger$.
As such, we will instead use
$ K_"C "^ddagger = 1/N_"A "^delta (Z^ddagger) / (Z_"AB" Z_"C ") exp(- (Delta U^ddagger) / (R T)), $<eq:Kc_tst>
where $Z_i$ is the molecular partition function per unit volume (i.e. $Z_i$ = $Z_i' \/V)$.
The need for $Z_i$ being in units of $"volume"^(-1)$ is necessary so that we arrive at the appropriate units for $K_"C "^ddagger$.
The factor of $N_"A "^(-delta)$, where $delta$ is the change in stoichiometric numbers (i.e. $delta = -1$ for this example), is included simply as a means of ensuring that $K_"C "^ddagger$ is in molar units rather than on a molecular basis.
It may seem that we have not made much progress here because we do not yet know how to calculate $Z_i$. Not to worry though, as that will be addressed below.

== Contributions to the Partition Function

The total molecular partition function for a species can be broken down into the products of vibrational, rotational, translational, and electronic partition functions.
Written mathematically,
$ Z = z_"trans"/V z_"rot" z_"vib" z_"el". $<eq:total_partition_function>
As we will show below, the natural place to introduce the units of inverse volume in $Z$ is with the translational partition function.
It should be noted that each of the translational, rotational, vibrational, and electronic partition functions can also be used to define a translational, rotational, vibrational, and electronic component to key thermodynamic properties like the enthalpy, entropy, and Gibbs free energy.
For the interested reader, we refer you to external sources on the subject matter.
#footnote["Section 10.3: Ensemble Properties and Basic Statistical Mechanics" in _Essentials of Computational Chemistry: Theories and Models_ (2#super[nd] ed.) by C.J. Cramer.]

=== Translational Partition Function

We now must define each of the partition functions.
The translational partition function derived from the particle-in-a-box model in quantum chemistry is given as
$ z_"trans" =  V / Lambda^d $
where $d$ is the number of dimensions available for translational motion (generally taken as $d=3$ but can be smaller for an adsorbate on a surface), $V$ is the volume of the container holding the molecule, and $Lambda$ is the de Broglie wavelength given by
$ Lambda equiv h / sqrt(2 pi m k_"B " T). $
Since we need $Z$ to have units of inverse volume, we will conveniently use
$ z_"trans" /V =  1 / Lambda^d. $
There is now no more volume term for us to worry about too, which is convenient.

=== Rotational Partition Function

// Put in rotational temperatures
Moving onward, we will now investigate the rotational partition function.
The rotational partition function is different depending on the shape of the molecule:
$
z_"rot" &= 1 quad ("monatomic")\
z_"rot" &= (8 pi^2 I k_"B " T) / (sigma h^2) quad ("linear")\
z_"rot" &= (8 pi^2 (8 pi^3 I_1 I_2 I_3)^(1/2) (k_"B "T)^(3/2)) / (h^3 sigma) quad ("nonlinear").
$
In these equations, $sigma$ represents the rotational symmetry number and is determined by the number of spatial orientations of the subject molecule that are identical.
For instance, $sigma$ is a value of 2 for linear molecules with a center of symmetry (e.g. a homonuclear diatomic molecule) and 1 for linear molecules without a center of symmetry (e.g. a heteronuclear diatomic molecule).
The quantity $I$ is the moment of inertia, and for the nonlinear case they are the three principal moments.
The moment of inertia is defined as
$ I equiv sum_j m_j r_(j)^2 $
where $m_j$ is the mass of atom $j$ and $r_j$ is the distance of atom $j$ to the axis of rotation.

=== Vibrational Partition Function

// Put in vibrational temperature
The vibrational partition function is given by
$ z_"vib" = product_(j=0)^N (1 - exp(- (h nu_j)/(k_"B " T)))^(-1), $
where $nu_j$ is the $j$-th vibrational frequency
#footnote[Vibrational spectra are normally reported in units of wavenumbers (inverse length). To convert a wavenumber $accent(nu,tilde)$ to a frequency, use $nu_j = c accent(nu,tilde)_j$.]
and the product is taken over all real vibrational modes in the system and $N$ is the number of vibrational modes given by
$
N=0 quad ("monatomic")\
N=3N_0 - 5 quad ("linear")\
N=3N_0 - 6 quad ("nonlinear").
$
A key point should be emphasized for transition states.
When a transition state forms, there is an imaginary vibrational mode connecting the minima to the transition state on the minimum energy pathway.
This means that for a transition state, $N$ should be one less than would otherwise be expected.

=== Electronic Partition Function

Finally, the electronic partition function is given by
$ z_"el" = sum_j g_i exp(- E_j / (k_"B " T) ) $
where $g_j$ is the degeneracy of electronic state $j$ and $E_j$ is the electronic energy above the ground state for electronic state $j$.
#footnote[It is worth clarifying a subtlety now that we understand how the electronic partition function is defined. Since we are taking the ground-state electronic configuration as being the zero-energy reference, there is a factor of $exp(-Delta E^ddagger \/ R T)$ that would be missing in #ref(<eq:Kc_tst>). Strictly, it is a factor of $exp(-Delta U^ddagger \/ R T)$ because there is always a zero-point energy present, which was also implicitly subtracted out in our definition of $z_"el"$.]
Most non-radical species have $q_"el" approx 1$ because the energetic states above the ground state are so much higher in energy that they do not contribute substantially to $q_"el"$.
However, this is not universally true. For instance, #ce("CH4^∙") has one unpaired electron and therefore has $g_0=2$ (i.e. the ground-state has a degeneracy of 2).
Additionally, the ground-state magnetic configuration of #ce("O2") has two unpaired electrons, such that $g_0=3$.
In most cases relevant to thermal heterogeneous catalysis, the excited states are sufficiently high in energy that $g_(j>0) approx 0 $.

== Rates of Reaction from Partition Functions <rates-of-reaction>

=== Transition State Theory Rate

With the partition function business out of the way, we will take a moment to revisit what we know about our rate expression:
$
r &= k conc("A") conc("B") \
r &= kappa_"f " (1-kappa_"f ") nu^ddagger K_"C "^ddagger conc("A") conc("B") \
r &= kappa_"f " (1-kappa_"f ") nu^ddagger 1/N_"A "^delta (Z^ddagger) / (Z_"AB" Z_"C ") exp(- (Delta U^ddagger) / (R T)) conc("A") conc("B").
$
We now know how to compute the partition functions now, but what do we do about $nu^ddagger$?
The motion along the minimum energy pathway through the transition state is along a vibrational mode representing the bond-breaking or bond-making event.
As alluded to previously, $nu^ddagger$ can be thought of as a vibrational frequency describing this event.
We know from the definition of the vibrational partition function that a single vibrational mode can be expressed as
$ z_"vib, TS mode" = (1 - exp(- (h nu^ddagger) / (k_"B " T)))^(-1) approx (k_"B "T) / (h nu^ddagger). $
where the latter approximation is made because $h nu^ddagger << k_"B " T$ in most cases.
#footnote[We have implicitly taken advantage of the Taylor expansion $exp(x) = 1 + x + x^2\/2! + x^3\/3! + ...$ and dropped the second-order and higher terms.]
With some rearrangement, we have
$ nu^ddagger = (k_"B " T) / h 1/ z_"vib, TS mode" $
The contribution from this (imaginary) vibrational mode was already removed in our description of $z_"vib"^ddagger$, and that is because it is instead accounted for here in the term for $nu^ddagger$.
We will drop $z_"vib, TS mode"$ as a result since its inclusion would only serve to indicate that a vibrational mode should be removed from $Z^ddagger$, which we have already done as a matter of convenience.

Finally, we arrive at
$ r = kappa_"f " (1-kappa_"f ") (k_"B "T)/h 1/N_"A "^delta (Z^ddagger) / (Z_"AB" Z_"C ") exp(- (Delta U^ddagger) / (R T)) conc("A") conc("B"). $<eq:tst_final>
Recall that the terms proceeding the concentrations can be thought of as $k$ in $r = k conc("A") conc("B")$.
in fact, $k$ here looks a lot like the Arrhenius equation if we say that
$
k &= A_0 exp(-(Delta U^ddagger)/(R T))\
A_0 &equiv kappa_"f " (1-kappa_"f ") (k_"B " T)/h 1/N_"A "^delta (Z_("AB")^ddagger) / (Z_"A " Z_"B ").
$<eq:tst_a>

It is worth noting that the conventional treatment of transition state theory will have a factor of $kappa$ but not $(1-kappa_"f ")$.
This discrepancy can be resolved by noting that if $kappa<<1$, then we are left with just the $kappa_"f "$ term.

Thinking back to the modified Arrhenius expressions with temperature-dependence terms on the prefactor as in #ref(<eq:arrhenius_mod>), we can see the $T^m$ dependence of the prefactor has $m=0$ for the empirical Arrhenius expression and $m=1$ from transition state theory.
In practice, however, the multiplicative $T$ factor is often negligible.
While we have been careful to not drop $kappa_"f "$ until this point, it should be noted that $kappa_"f "$ is often conveniently assumed to have a value of 


=== Applications to Elementary Processes

As was shown previously, the rate coefficient for a given elementary step is related to the ratio of partition functions for the transition state and the reactant(s).
If one considers a free molecule (e.g. in the gas phase), it has three translational degrees of freedom, three rotational degrees of freedom if nonlinear or two rotational degrees of freedom if linear, and several vibrational degrees of freedom depending on the number of atoms $N_0$.
We will represent this as follows, where the exponents in quotes are simply how many degrees of freedom there are.#footnote[We have tacitly assumed that the excited states are energetically negligible, such that $z_"el" = g_0$ and therefore $z_"el"$ has only one degree of freedom.]
$ Z = z_"trans"^((3)) z_"rot"^((a)) z_"vib"^((N)) z_"el" $
where
$
a=0, quad N &=0 quad ("monatomic")\
a=2, quad N &= 3N_0-5 quad ("linear")\
a=3, quad N &=3N_0-6 quad ("non-linear").
$

For a transition state, one vibrational degree of freedom is lost, such that
$ Z^ddagger = z_"trans"^((3)) z_"rot"^((a)) z_"vib"^((N-1)) z_"el" $

When a molecule adsorbs onto a surface, some of these degrees of freedom are lost.
If the adsorbate is strongly chemisorbed onto the surface, then there are likely no translational or rotational degrees of freedom left.
The vibrational degrees of freedom are still present, although the vibrational modes are likely to differ substantially from the gas phase, and there are now $3N_0$ modes.
These approximations are often known as the harmonic limit.
If the adsorbate is somewhat weakly bound, then the remaining degrees of freedom are likely to be somewhere between that used for a free gas and that of a chemisorbed species.
More complicated expressions are available to model these intermediate behaviors, such as the hindered translator--hindered rotor model.
#footnote[L.H. Sprowl, C.T. Campbell, L. Árnadóttir, "Hindered Translator and Hindered Rotor Models for Adsorbates: Partition Functions and Entropies," _J. Phys. Chem. C_, 120, 9719--9731 (2016).]

== A Macroscopic, Thermodynamic Perspective

=== Eyring Equation <thermodynamic-perspective>

If we assume ideal conditions hold, we can state that
$ Delta G^std^ddagger = - R T ln(K_"C "^ddagger /C^std^delta), $
such that
$ K_"C "^ddagger = C^std^delta exp(- (G^std^ddagger) / (R T)). $
Here, $C^std$ is the standard-state reference concentration (usually given by $P\/(R T)$ at 1 bar for gases or as the molal concentration of pure components for liquids) coming from the fact that we are using concentrations instead of activities.
Plugging this expression into the definition of $k$ in PLACEHOLDER yields
$ k = (k_"B "T )/h C^std^(delta) exp(- (Delta G^std^ddagger) / (R T)). $
Naturally, when taking advantage of the thermodynamic relationship
$ Delta G^std = Delta H^std - T Delta S^std $
this becomes
$ k = (k_"B "T )/h C^std^(delta) exp(( Delta S^std^ddagger) / R) exp(-(Delta H^std^ddagger) / (R T)), $ <eq:eqyring-final>
This expression is known as the Eyring equation.
For the sake of clarity, the net rate of reaction also be written out in full as
$ r = (k_"B "T )/h C^std^(delta) exp(( Delta S^std^ddagger) / R) exp(-(Delta H^std^ddagger) / (R T)) conc("A") conc("B"). $
For the sake of illustration, we can separate out the various terms in $k$ in analogy with the Arrhenius equation to arrive at
$
k &= A_0 exp(- (Delta H^std^ddagger) / (R T))\
A_0 &equiv (k_"B " T)/h C^std^delta exp((Delta S^std^ddagger)/R).
$

A natural question arises here: how does $Delta H^std^ddagger$ related to $Delta U^ddagger$ in #ref(<eq:tst_final>)?
From fundamental thermodynamics, we know that
$ Delta H^std^ddagger = Delta U^ddagger + P Delta V^ddagger. $
For solids and liquids, $Delta U^ddagger >> P Delta V^ddagger$, such that $Delta H^std^ddagger = Delta U^ddagger$. For gases, if we assume that the ideal gas law holds, then $P Delta V^ddagger = Delta n^ddagger R T,$ such that $Delta H^std^ddagger = Delta U^ddagger + Delta n^ddagger R T$.
