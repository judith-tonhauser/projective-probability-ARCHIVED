# Projective probability

## Goal

To investigate the hypothesis that the projectivity of content is influenced by the prior probability of the event described by the clause that conveys the content: higher probability results in higher projectivity. To achieve this goal, we manipulate the prior probability of events described by clauses. To investigate the hypothesis, we rely on a particular kind of projective content, namely the content of the complement of attitude predicates. Here, we also control for whether/the extent to which the content of the complement is entailed, to also explore how the extent to which the complement is entailed interacts with the effect of prior probability on projectivity.

## Prior and posterior probability of eventualities

- Eventualities are described by sentences like "Mary is pregnant”.
- Interlocutors (in a discourse) have prior probability (belief) distributions over eventualities: e.g., the prior probability of the eventuality described by “Mary is pregnant” is an interlocutor’s subjective belief that this state holds (at the topic time) before considering some evidence (e.g., hearing a sentence that describes the eventuality). 
- The prior probability distribution (or: prior) of an eventuality is context-dependent: e.g., the state of Mary being pregnant is lower if Mary is an 8-year old girl than if Mary is a 25-year old woman.
- The prior of an eventuality may also be interlocutor-dependent: e.g., A’s knowledge about Mary may make A assign a higher prior to the state of Mary being pregnant than B’s knowledge about Mary.
- In this experiment, we use context to control the prior of eventualities: e.g., Context 1 is a high probability context for the state of Mary being pregnant, whereas Context 2 is a low probability context for the same state.	
	- Context 1: Mary just walked into an OBGYN’s office.
	- Context 2: Mary is a 6-year old girl.
- In this experiment, we are interested in the posterior probability distribution that listeners (the relevant interlocutors) have for eventualities after taking into account the relevant evidence, which is i) the context, and ii) the utterance (in that context) of a sentence that describes the eventuality.
- The uttered sentences are polar questions in which an attitude predicate (with a third person subject) embeds a clause that describes the relevant eventuality. The attitude predicates differ on two dimensions: i) the extent to which the content of the clausal complement is entailed, and ii) the extent to which the content of the clausal complement is projective. (These two dimensions are not independent: more projective complements are also more likely to be entailed.)
- Thus, more specifically, the posterior probability distribution that listeners have for eventualities depends on i) the context, and ii) two properties of the attitude predicate of the uttered sentence. 

## Background on entailment

- Entailment is a relationship between two sentences, or between a sentence and a proposition: A entails B iff every situation in which A is true is a situation in which B is true. 
- In semantics, entailment is typically considered to be a binary, categorical relationship.
- For attitude predicates, however, some remarks have been made that suggest that whether content of the clausal complement is entailed may not be categorical, or at least not easily determined.
	- Schlenker (2010:139) about “announce”: “in some contexts, it does not entail the truth of its complement; in other contexts, it entails and presupposes the truth of its complement.” For instance, he suggested that whether the content of the complement of "announce" is judged to be entailed depends on who the attitude holder is; e.g., “Mary announced that she is pregnant” seems to be judged to entail that Mary is pregnant if Mary is a sane, adult woman but not if Mary is 8 years old.
	- Sudo (2012): Ph.D. thesis supposedly argues that some triggers don’t entail the presupposition (haven’t worked through this)
	- Swanson (2012) doesn't take the content of the complement of "establish" to be entailed though other speakers might argue that “The detective established that Margret didn’t kill James” entails that Margret didn’t kill James.
Thus, exploring the extent to which different attitude predicates entail the content of the complement is an interesting question of its own.
- With respect to projectivity, entailment is also relevant because some authors take only entailed meanings to project (or to be presuppositions). Non-entailed meanings that speakers are taken to be committed to are considered a completely different phenomenon (e.g., Anand & Hacquard 2014: “illusion of factivity” and “illusion of projection”). Including attitude predicates that differ in the extent to which the clausal complement is entailed will thus also allow us to explore whether the extent to which the projectivity of the complement is influenced by the prior of the eventuality described by the complement differs with the extent to which the clausal complement is entailed (i.e., we ask: is there an interaction between prior probability and entailment).

## Three separate experiments
- entailment 
- prior
- projectivity

## Linear mixed-effects model we want to run

	projectivity ~ prior * entailment + (1 + prior * entailment | participant) + (1 + prior | predicate)

where
- projectivity: individual participants’ projectivity ratings between 0 and 1, of utterance in contexts that manipulates the prior probability of the eventuality described by the content of the complement 
- prior: prior probability distribution of the eventuality when it is described by a main clause in contexts that manipulate the prior probability of the eventuality, this distribution is collected in a separate experiment
		[is this possible, or do we just take the mean prior probability?]
- entailment: mean rating (between 0 and 1) about the extent to which the content of the clausal complement of the attitude predicate is entailed, these ratings are collected in a separate experiment

Motivation for including interaction term: We hypothesize that projectivity is predicted by the prior. According to classical analyses of presuppositions, non-entailed complements do not project, they at best give the “illusion of projection”. We, however, hypothesize that the projectivity of non-entailed complements will be influenced by the prior, too. We include the interaction term because we want to identify whether the prior’s influence on the projectivity of the complement differs for entailed and non-entailed complements. 

[We had talked about getting prior and projectivity ratings in the same experiment, but I now worry that participants’ prior ratings may be influenced by their projectivity ratings, and vice versa. On the other hand, I did not worry about this in the at-issue/projectivity rating, so why do I worry now? An advantage of running 2 pre-tests (entailment, prior) and then a main projectivity experiment is that we can include more predicates.] 

[Perhaps also include as fixed effects: tense of attitude predicate?]

## Materials

	- 20 attitude predicates
	- 20 eventualities / clauses that describe these eventualities
	- 2 contexts per eventuality (one higher and one lower probability; it’s OK if not at ceiling/bottom and there’s a mix of probabilitities across the eventuality/contexts pairings)

## Entailment experiment

- Goal: to identify the extent to which the content of the clausal complement of an (unembedded) attitude predicate is entailed
- Response task: Given the standard definition of entailment (see above), we need a task that probes the extent to which the attitude sentence can be true and the content of the clausal complement false, i.e., the extent to which the content of the clausal complement is not entailed by the attitude sentence. 
- Contradiction task: 
	- Bill: Steve believes/announced/discovered/knows that Marge is pregnant, but I know that she isn’t. 
	- Does Bill sound like he is contradicting himself?
	- Response on slider from 0/no to 1/yes
- For each predicate, we calculate the mean entailment rating, and use that in the model we want to run.
- Materials: atomic sentences with 20 attitude predicates, 20 clausal complements per predicate
	==> 400 stimuli total 
- Each participant sees each attitude predicate with a unique clausal complement
(If we want 10 judgments per combination, we’d need 4,000 judgments)

## Prior probability distribution experiment

- Goal: to identify the prior probability distribution of the eventuality described by a main clause in a context.
- Response task: Participants assess the prior probability of the eventuality described by a main clause in a context.
	Context 1: Mary just walked into an OBGYN’s office.
	Context 2: Mary is a 6-year old girl.
	How likely is it that Mary is pregnant?
	Response on slider from 0/very unlikely to 1/highly likely
- For each event, we calculate the mean likeliness / the prior probability distribution [see my question above]
- Materials: 20 atomic main clauses that lexicalize the eventualities, presented in one of two contexts
	==> 40 stimuli total
- Each participant rates the prior for 20 unique eventualities in one context 

## Projectivity experiment

- Goal: to identify the extent to which participants take the speaker to be committed to the content of the clausal complement of an attitude predicate realized in a polar question and uttered in a context.
- Response task: “certain that” diagnostic	
	Context 1: Mary just walked into an OBGYN’s office.
	Context 2: Mary is a 6-year old girl.
	Bill: Did Sue discover that Mary is pregnant?
	Is Bill certain that Mary is pregnant?
	Response on a slider from 0/no to 1/yes
- Materials: 20 polar questions formed from sentences with attitude predicates that embed a clausal complement, in context
	==> 800 stimuli (20 predicates x 20 complements x 2 contexts)
- Each participant rates the projectivity of the content of the clausal complement for each attitude predicate with a unique clausal complement in a unique context.

## Stimuli:
	
20 predicates (and reasons for inclusion): 9 projective, 7 non-projective, 4 somewhat projective
	- annoyed (variability paper, classical emotive, highest projectivity)
	- know (variability paper, classical trigger, highly projective in variability paper, database: 1.5)
	- discover (variability paper, classical trigger, high projectivity but some variability)
	- establish (variability paper, lowest projectivity; Swanson 2012: entailed but not projective)
	- confess (variability paper, 2nd lowest projectivity, “falsely confessed”, Swanson 2012: not entailed)
	- reveal (variability paper, 3rd lowest projectivity)
	- see (variability paper, sensory factive, highly projective, more variability than “annoyed”, database: 2.2)
	- hear (sensory factive, database: 1.2)
	——— 8 (mostly projective)
	- announce (Schlenker 2010 “part-time trigger”, database: 1, variable projectivity)
	- inform Sam (pair with “announce” in Schlenker 2010, same meaning but entails and presupposes complement)
	- be right that (compared with “know” in much research: entails complement but no presupposition)
	- think (classical non-trigger, doesn’t entail or presuppose complement, database: -0.5)
	——— 4 (2 non-projective, 1 projective, 1 somewhat projective)
	- acknowledge (assertive, A&H 2014: “illusion of factivity”, i.e., non-veridical and non-factive)
	- admit (assertive, A&H 2014: “illusion of factivity”, i.e., non-veridical and non-factive, database: 1)
	- confirm (assertive, A&H 2014: “illusion of factivity”, i.e., non-veridical and non-factive)
	——— 3 (3 somewhat projective)
	- prove (A&H 2014: non-veridical (even though at first appear to be) and non-factive, database: 0.5)
	- demonstrate (A&H 2014: non-veridical (even though at first appear to be) and non-factive)
	——— 2 (2 non-projective)
	- pretend (lowest in database: -1.5) DIFFICULT TO PRETEND THINGS?
		[perhaps replace with imagine, database: -0.3]
	- say (A&H 2012: neither veridical nor factive, database: 0.8)
	- suggest (database: 2nd lowest, -0.8)
	——— 3 (3 non-projective)

## 20 eventualities (described by main or embedded clauses) and two contexts each
	[10 women’s names / 10 men’s names]
	- Mary is pregnant (Mary is a 6-year old girl / Mary just walked into Planned Parenthood)
	- Josie went on vacation to France (Josie doesn’t have a passport / Josie loves France)
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
	- Jayden rented a car (Jayden doesn’t have a driver’s license / Jayden’s car is in the shop)
	- Tony had a drink (Tony has been sober for 20 years / Tony went to a bar)
	- Josh learned to ride a bike (Josh is a 5-year old boy / Josh is a 75-year old man)
	- Owen shoveled snow (Owen lives in Chicago / Owen lives in New Orleans)
	- Julian danced salsa (Julian is from Germany / Julian is from Cuba)
	- Jon walked to work (Jon lives 2 blocks away from his work / Jon lives 10 miles away from his work)
	- Charley speaks Spanish (Charley lives in Korea / Charley lives in Mexico)







