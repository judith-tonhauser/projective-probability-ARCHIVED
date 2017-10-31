# Projective probability

## Goal

To investigate the hypothesis that the projectivity of clausal content is influenced by the prior probability of the event described by the clause that conveys the content: higher probability of the event results in higher projectivity of the content. 

To investigate the hypothesis, we manipulate the prior probability of events described by clauses and investigate the projectivity of the content of those clauses.  This project follows up on the finding reported in our Journal of Semantics paper that (what we called) 'lexical content' is a predictor of projectivity. 

The projective content that we use to investigate the hypothesis is the content of clausal complements of attitude predicates: we hypothesize that higher probability of the events described by those clauses results in higher projectivity of the content of those clauses. Since the extent to which the content of the clausal complement of the attitude predicate is entailed content may influence projectivity, we also control for the extent to which the content of the complement is entailed. We will thereby also be able to identify how the extent to which the complement is entailed interacts with the effect of prior probability on projectivity.

## Prior and posterior probability of eventualities

- Eventualities are described by sentences like "Mary is pregnant".
- Interlocutors (in a discourse) have prior probability (belief) distributions over eventualities: e.g., the prior probability of the eventuality described by "Mary is pregnant" is an interlocutor's subjective belief that this state holds (at the topic time) before considering some evidence (e.g., hearing a sentence that describes the eventuality). 
- The prior probability (or: prior) of an eventuality depends on world knowledge: e.g., the state of Mary being pregnant is lower if Mary is an 8-year old girl (and the interlocutor whose prior we are concerned with is aware of this) than if Mary is a 25-year old woman (and the interlocutor whose prior we are concerned with is aware of this).
- In this experiment, we control the prior of eventualities by presenting information about the world, in the form of 'facts': e.g., given Fact 1, the state of Mary being pregnant has a higher probability than given Fact 2. 
	
	- Fact 1: Mary just walked into an OBGYN's office.
	
	- Fact 2: Mary is a 6-year old girl.
	
- In this experiment, we are interested in the posterior probability distribution that listeners (the relevant interlocutors) have for eventualities after taking into account the relevant evidence, which are i) facts about the world, and ii) utterance (in given a fact) of a sentence that describes the eventuality (including the speaker's choice of attitude predicate).
- The uttered sentences are polar questions in which an attitude predicate (with a third person subject) embeds a clause that describes the relevant eventuality. The attitude predicates differ on two dimensions: i) the extent to which the content of the clausal complement is entailed, and ii) the extent to which the content of the clausal complement is projective. (These two dimensions are not independent: more projective complements are also more likely to be entailed.)
- Thus, more specifically, the posterior probability distribution that listeners have for eventualities depends on i) facts about the world, and ii) two properties of the attitude predicate of the uttered sentence. 

## Background on entailment

- Entailment is a relationship between two sentences, or between a sentence and a proposition: sentence A entails sentence/proposition B iff every situation in which A is true is a situation in which B is true. 
- In semantics, entailment is typically considered to be a binary, categorical relationship.
- For attitude predicates, however, some remarks have been made that suggest that whether content of the clausal complement is entailed may not be categorical or, even if it is categorical, may not always be straightforwardly determined. In particular, some predicates that at first appear to be veridical (i.e., entail the content of the complement) are not so, on closer inspection.
	- Schlenker (2010:139) about "Äúannounce"Äù: "Äúin some contexts, it does not entail the truth of its complement; in other contexts, it entails and presupposes the truth of its complement."Äù For instance, he suggested that whether the content of the complement of "announce" is judged to be entailed depends on who the attitude holder is; e.g., "ÄúMary announced that she is pregnant"Äù seems to be judged to entail that Mary is pregnant if Mary is a sane, adult woman but not if Mary is 8 years old.
	- Sudo (2012): Ph.D. thesis supposedly argued that some triggers don't entail the presupposition (haven't worked through this)
	- Swanson (2012) didn't take the content of the complement of "establish" to be entailed though other speakers might argue that "ÄúThe detective established that Margret didn't kill James"Äù entails that Margret didn't kill James.
Thus, exploring the extent to which the content of the complement of different attitude predicates is entailed is an interesting question of its own.
- With respect to projectivity, entailment is also relevant because some authors take only entailed meanings to project (or to be presuppositions). Non-entailed meanings that speakers are taken to be committed to are considered a completely different phenomenon (e.g., Anand & Hacquard 2014: "Äúillusion of factivity"Äù and "Äúillusion of projection"Äù). Including attitude predicates that differ in the extent to which the clausal complement is entailed will thus also allow us to explore whether the extent to which the projectivity of the complement is influenced by the prior of the eventuality described by the complement differs with the extent to which the clausal complement is entailed (i.e., we ask: is there an interaction between prior probability and entailment).

## Three separate experiments
1. prior
2. entailment 
3. projectivity

## Linear mixed-effects model we want to run

	projectivity ~ prior * entailment + (1 + prior * entailment | participant) + (1 + prior | predicate)

where
- projectivity: individual participants' projectivity ratings between 0 and 1, of utterances given facts about the world that manipulates the prior probability of the eventuality described by the content of the complement 
- prior: mean prior probability rating (between 0 and 1) of the eventuality when it is described by a main clause given facts about the world that manipulate the prior probability of the eventuality; we collect this prior ratings in a separate experiment 
- entailment: mean veridicality rating (between 0 and 1), i.e., the extent to which the content of the clausal complement of the attitude predicate is entailed, these ratings are collected in a separate experiment

Motivation for including interaction term: We hypothesize that projectivity is predicted by the prior. According to classical analyses of presuppositions, non-entailed complements do not project, they at best give the "Äúillusion of projection"Äù. We, however, hypothesize that the projectivity of non-entailed complements will be influenced by the prior, too. We include the interaction term because we want to identify whether the prior's influence on the projectivity of the complement differs for entailed and non-entailed complements. 

[Perhaps also include as fixed or random effects: tense of attitude predicate?]

## Materials
- 20 attitude predicates (cross-balancing for projectivity and veridicality)
- 20 non-gradable, non-subjective eventualities (as described by main clauses)
- 2 facts about the world per eventuality (one in which the eventuality has a higher prior probability, though not at ceiling, and one in which the eventuality has a lower prior probability, though not at floor)

## Prior probability experiment

- Goal: to identify readers' prior probabilities of the eventualities described by main clauses, given a fact about the world
- Response task: Participants assess the likeliness of the eventuality described by a main clause, given a fact about the world
	- Fact: Mary just walked into Planned Parenthood.
	  
	  How likely is it that Mary is pregnant?
	- Fact: Mary is a high school student.
	  
	  How likely is it that Mary is pregnant?
- Response on slider from 0/very unlikely to 1/very likely
- For each eventuality, we calculate the mean likeliness, i.e., the mean prior probability: the higher the mean likeliness, the higher the prior probability of the eventuality.
- Materials: 20 atomic main clauses that lexicalize the eventualities, presented with one of two facts each, i.e., 40 stimuli total
- Each participant rates the likeliness of 20 unique eventualities, given a fact about the world.
- We want at least 20 ratings per eventuality/fact combination.

## Entailment experiment

- Goal: to identify the extent to which the content of the clausal complement of an (unembedded) attitude predicate is entailed
- The clausal complements are the 20 clauses for which we established facts in the prior probability experiment that result in the eventualities described by these clauses to have a higher versus a lower prior.
- Response task: Given the standard definition of entailment (see above), we need a task that probes the extent to which the attitude sentence can be true and the content of the clausal complement false, i.e., the extent to which the content of the clausal complement is not entailed by the attitude sentence. 
- Contradiction task: 
	
	Bill: Steve believes/announced/discovered/knows that Marge is pregnant, but I know that she isn't. 
	
	Does Bill sound like he is contradicting himself?
- Response on slider from 0/no to 1/yes
- For each predicate, we calculate the mean contradiction rating: the higher the mean contradiction rating, the more veridical the predicate is.
- Materials: atomic sentences with 20 attitude predicates, 20 clausal complements per predicate 
	
	==> 400 stimuli total 
- Each participant sees each attitude predicate with a unique clausal complement. If we want 10 ratings per combination, we need 4,000 ratings total.

## Projectivity experiment

- Goal: to identify the extent to which participants take the speaker to be committed to the content of the clausal complement of an attitude predicate realized in a polar question, given a fact about the world.
- Response task: "certain that" diagnostic (from Journal of Semantics paper)
	- Fact: Mary just walked into Planned Parenthood.
	  
	  Bill: Did Sue discover that Mary is pregnant?
	
	  Is Bill certain that Mary is pregnant?
- Response on a slider from 0/no to 1/yes
- For each predicate/clause/fact triple, we calculate the mean certainty rating: the higher the mean certainty rating, the higher the projectivity of the content of the clausal complement.
- Materials: 20 polar questions formed from sentences with attitude predicates that embed a clausal complement, given a fact

==> 800 stimuli (20 predicates x 20 complements x 2 facts)
- Each participant rates the projectivity of the content of the clausal complement for each attitude predicate with a unique clausal complement given a unique fact.

## Stimuli:
	
**20 predicates (and reasons for inclusion)**

| Predicate  | reason for inclusion  | projective?   | veridical?  |
|---|---|---|---|
|  annoyed |  variability paper, classical emotive, highest projectivity |  md-checkmark | md-checkmark  |
|  know  |  variability paper, classical trigger, highly projective in variability paper, database: 1.5 |   |   |
|  discover |  variability paper, classical trigger, high projectivity but some variability |   |   |
|  establish  | variability paper, lowest projectivity; Swanson 2012: entailed but not projective  |   |   |
|  confess | variability paper, 2nd lowest projectivity, "Äúfalsely confessed"Äù, Swanson 2012: not entailed  |   |   |
|  reveal  | variability paper, 3rd lowest projectivity  |   |   |
|  see | variability paper, sensory factive, highly projective, more variability than "Äúannoyed"Äù, database: 2.2  |   |   |
|  hear | sensory factive, database: 1.2  |   |   |
|  announce | Schlenker 2010 "Äúpart-time trigger"Äù, database: 1, variable projectivity  |   |   |
|  inform Sam |  pair with "Äúannounce"Äù in Schlenker 2010, same meaning but entails and presupposes complement |   |   |
|  be right that |  compared with "Äúknow"Äù in much research: entails complement but no presupposition |   |   |
| think  | classical non-trigger, doesn't entail or presuppose complement, database: -0.5   |   |   |
|   |   |   |   |
|   |   |   |   |
|   |   |   |   |
|   |   |   |   |
|   |   |   |   |
|   |   |   |   |
|   |   |   |   |
|   |   |   |   |

"Äî"Äî"Äî 4 (2 non-projective, 1 projective, 1 somewhat projective)
- acknowledge (assertive, A&H 2014: "Äúillusion of factivity"Äù, i.e., non-veridical and non-factive)
- admit (assertive, A&H 2014: "Äúillusion of factivity"Äù, i.e., non-veridical and non-factive, database: 1)
- confirm (assertive, A&H 2014: "Äúillusion of factivity"Äù, i.e., non-veridical and non-factive)
"Äî"Äî"Äî 3 (3 somewhat projective)
- prove (A&H 2014: non-veridical (even though at first appear to be) and non-factive, database: 0.5)
- demonstrate (A&H 2014: non-veridical (even though at first appear to be) and non-factive)
"Äî"Äî"Äî 2 (2 non-projective)
- pretend (lowest in database: -1.5) DIFFICULT TO PRETEND THINGS?
[perhaps replace with imagine, database: -0.3]
- say (A&H 2012: neither veridical nor factive, database: 0.8)
- suggest (database: 2nd lowest, -0.8)
"Äî"Äî"Äî 3 (3 non-projective)

**projectivity: 9 projective, 7 non-projective, 4 somewhat projective

## 20 eventualities (described by main or embedded clauses) and two facts about the world each
**10 women's names / 10 men's names**
- Mary is pregnant (Mary is a 6-year old girl / Mary just walked into Planned Parenthood)
- Josie went on vacation to France (Josie doesn't have a passport / Josie loves France)
- Emma gained weight (Emma is on a strict diet / Emma is a 4-month old baby)
- Olivia slept in (Olivia has two small children / Olivia is a bar tender)
- Sophia got a tattoo (Sophia is a high end fashion model / Sophia is a hipster)
- Mia got drunk last night (Mia is a nun / Mia is a college student)
- Isabella ate a steak (Isabella is a vegetarian / Isabella is from Argentina)
- Emily bought a car (Emily is broke / Emily has been saving for a year)
- Grace visited her sister (Emily hates her sister / Emily loves her sister)
- Zoe calculated the tip (Zoe is 2 years old / Zoe is a math major)
- Danny ate the cupcake (Danny is a diabetic / Danny has frosting on his face)
- Frank got a cat (Frank is allergic to cats / Frank has always wanted a pet)
- Jackson ran 10 miles (Jackson is overweight / Jackson is training for a marathon)
- Jayden rented a car (Jayden doesn't have a driver's license / Jayden's car is in the shop)
- Tony had a drink (Tony has been sober for 20 years / Tony went to a bar)
- Josh learned to ride a bike (Josh is a 5-year old boy / Josh is a 75-year old man)
- Owen shoveled snow (Owen lives in Chicago / Owen lives in New Orleans)
- Julian danced salsa (Julian is from Germany / Julian is from Cuba)
- Jon walked to work (Jon lives 2 blocks away from his work / Jon lives 10 miles away from his work)
- Charley speaks Spanish (Charley lives in Korea / Charley lives in Mexico)







