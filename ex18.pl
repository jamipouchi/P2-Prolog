/*
Consider two groups of 10 people each. In the first group, as expected, the percentage of people
with lung cancer among smokers is higher than among non-smokers. In the second group, the
same is the case. But if we consider the 20 people of the two groups together, then the situation
is the opposite: the proportion of people with lung cancer is higher among non-smokers than
among smokers! Can this be true? Write a little Prolog program to find it out.
*/

% It gives false! So, it cannot be true. Let's do some math to understand why.

/*
s1 = smokers in group 1
c1 = lung cancer in group 1
s2 = smokers in group 2
c2 = lung cancer in group 2

s1/c1 > (10 - s1)/c1 && s2/c2 > (10 - s2)/c2

- Multiply first equation by (c1 + c2) / c1 && Multiply second equation by (c1 + c2) / c2
=>
s1/(c1 + c2) > (10 - s1)/(c1 + c2) && s2/(c1 + c2) > (10 - s2)/(c1 + c2)

- Add the two equations
=>
(s1 + s2) / (c1 + c2) > (20 - s1 - s2) / (c1 + c2)

Thus, if we have more proportion of lung cancer given smoker in each group, the global proportion of lung cancer given smoker is higher than non-smoker.
*/

group(1, 10).
group(2, 10).

smokers(GROUP, NUM_SMOKERS, NUM_NON_SMOKERS) :- 
    group(GROUP, People), 
    between(1, People, NUM_SMOKERS),                                    % at least 1 smoker, to fulfill the condition of the problem
    NUM_NON_SMOKERS is People - NUM_SMOKERS.

lung_cancer(GROUP, NUM_LUNG_CANCER) :- 
    group(GROUP, People), 
    between(1, People, NUM_LUNG_CANCER).                                % at least 1 lung cancer, to fulfill the condition of the problem

check_perc(NUM_SMOKERS, NUM_NON_SMOKERS, NUM_LUNG_CANCER) :- 
    PERC_LUNG_CANCER_GIVEN_SMOKER is NUM_SMOKERS / NUM_LUNG_CANCER, 
    PERC_LUNG_CANCER_GIVEN_NON_SMOKER is NUM_NON_SMOKERS / NUM_LUNG_CANCER,
    PERC_LUNG_CANCER_GIVEN_SMOKER > PERC_LUNG_CANCER_GIVEN_NON_SMOKER.

two_group_perc_lung_cancer(GROUP1, GROUP2) :-
    smokers(GROUP1, NUM_SMOKERS1, NUM_NON_SMOKERS1),                    % smokers for group 1
    lung_cancer(GROUP1, NUM_LUNG_CANCER1),                              % lung cancer for group 1
    check_perc(NUM_SMOKERS1, NUM_NON_SMOKERS1, NUM_LUNG_CANCER1),       % check if  perc of lung cancer given smoker is higher than perc of lung cancer given non-smoker in group 1
    smokers(GROUP2, NUM_SMOKERS2, NUM_NON_SMOKERS2),                    % smokers for group 2
    lung_cancer(GROUP2, NUM_LUNG_CANCER2),                              % lung cancer for group 2
    check_perc(NUM_SMOKERS2, NUM_NON_SMOKERS2, NUM_LUNG_CANCER2),       % check if  perc of lung cancer given smoker is higher than perc of lung cancer given non-smoker in group 2
    TOTAL_SMOKERS is NUM_SMOKERS1 + NUM_SMOKERS2,                       % total smokers
    TOTAL_NON_SMOKERS is NUM_NON_SMOKERS1 + NUM_NON_SMOKERS2,           % total non-smokers
    TOTAL_NUM_CANCER is NUM_LUNG_CANCER1 + NUM_LUNG_CANCER2,            % total lung cancer
    \+ check_perc(TOTAL_SMOKERS, TOTAL_NON_SMOKERS, TOTAL_NUM_CANCER).  % check if  NOT perc of lung cancer given smoker is higher than perc of lung cancer given non-smoker in total
