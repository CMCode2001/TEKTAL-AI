% ======= RÈGLES D'INFÉRENCE AVEC GESTION DES EXCEPTIONS =======

% 1. ÉVALUATION DE LA COMPATIBILITÉ MÉTIER/PROFIL
% Règle principale de compatibilité qui génère un score global
compatible_avec_metier(Personne, Metier, Score, Details) :-
    % Vérification du niveau d'études
    (niveau_etude(Personne, NiveauEtude) ->
        true
    ;
        throw(error(missing_data, 'Niveau d\'études non défini pour la personne'))
    ),
    (niveau_etude_requis(Metier, NiveauRequisEtude) ->
        true
    ;
        throw(error(missing_data, 'Niveau d\'études requis non défini pour le métier'))
    ),
    format('~n=== Évaluation de la compatibilité pour ~w ===~n', [Metier]),
    format('Niveau d\'études actuel: ~w (Requis: ~w)~n', [NiveauEtude, NiveauRequisEtude]),

    % Calcul des différents scores
    (calcul_score_niveau_etude(NiveauEtude, NiveauRequisEtude, ScoreEtude) ->
        format('Score niveau d\'études: ~w/100~n', [ScoreEtude])
    ;
        throw(error(calculation_error, 'Erreur dans le calcul du score niveau d\'études'))
    ),

    (calcul_score_competences(Personne, Metier, ScoreCompetences, CompDetails) ->
        format('Score compétences: ~w/100~n', [ScoreCompetences]),
        afficher_details_competences(CompDetails)
    ;
        throw(error(calculation_error, 'Erreur dans le calcul du score compétences'))
    ),

    (calcul_score_interet(Personne, Metier, ScoreInteret) ->
        format('Score intérêt: ~w/100~n', [ScoreInteret])
    ;
        throw(error(calculation_error, 'Erreur dans le calcul du score intérêt'))
    ),

    (calcul_score_personnalite(Personne, Metier, ScorePersonnalite) ->
        format('Score personnalité: ~w/100~n', [ScorePersonnalite])
    ;
        throw(error(calculation_error, 'Erreur dans le calcul du score personnalité'))
    ),

    % Calcul du score global pondéré
    Score is (ScoreEtude * 0.2 + ScoreCompetences * 0.4 + ScoreInteret * 0.25 + ScorePersonnalite * 0.15),
    format('Score global: ~w/100~n', [Score]),

    % Compilation des détails pour l'explication
    Details = details(
        etude(ScoreEtude, NiveauEtude, NiveauRequisEtude),
        competences(ScoreCompetences, CompDetails),
        interet(ScoreInteret),
        personnalite(ScorePersonnalite)
    ).

% Afficher les détails des compétences
afficher_details_competences(Details) :-
    format('Détails des compétences:~n', []),
    forall(member(detail(Comp, NiveauRequis, NiveauPersonne, ScoreComp), Details),
        format('  - ~w: Niveau actuel = ~w, Niveau requis = ~w, Score = ~w/100~n', [Comp, NiveauPersonne, NiveauRequis, ScoreComp])).

% Calcul du score relatif au niveau d'études
calcul_score_niveau_etude(NiveauActuel, NiveauRequis, Score) :-
    NiveauActuel >= NiveauRequis,
    Score = 100, !.
calcul_score_niveau_etude(NiveauActuel, NiveauRequis, Score) :-
    Difference is NiveauRequis - NiveauActuel,
    Score is max(0, 100 - (Difference * 25)).

% Calcul détaillé du score basé sur les compétences avec explications
calcul_score_competences(Personne, Metier, ScoreGlobal, Details) :-
    findall(detail(Comp, NiveauRequis, NiveauPersonne, ScoreComp), (
        requiert_competence(Comp, Metier, NiveauRequis),
        niveau_competence(Personne, Comp, NiveauPersonne),
        ScoreComp is min(NiveauPersonne / NiveauRequis, 1) * 100
    ), Details),
    
    % Calcul du score global
    findall(S, member(detail(_, _, _, S), Details), Scores),
    (Scores = [] -> 
        ScoreGlobal = 0
    ;
        sum_list(Scores, Total),
        length(Scores, N),
        ScoreGlobal is Total / N
    ).

% Calcul du score basé sur les intérêts
calcul_score_interet(Personne, Metier, Score) :-
    metier(Metier, Domaine),
    (interet(Personne, Domaine, NiveauInteret) ->
        Score is NiveauInteret * 20  % Convertir une échelle 1-5 en pourcentage
    ;
        Score = 0
    ).

% Calcul du score basé sur la personnalité
calcul_score_personnalite(Personne, Metier, Score) :-
    % Récupération des traits de personnalité requis pour le métier
    findall(ScoreTrait, (
        trait_personnalite_requis(Metier, Trait, ImportanceTrait),
        (trait_personnalite(Personne, Trait, ValeurTrait) ->
            ScoreTrait is (ValeurTrait / 5) * ImportanceTrait * 20
        ;
            ScoreTrait = 0
        )
    ), ScoresTraits),
    
    % Calcul du score global de personnalité
    (ScoresTraits = [] ->
        Score = 50  % Valeur par défaut si aucune donnée disponible
    ;
        sum_list(ScoresTraits, Total),
        length(ScoresTraits, N),
        Score is Total / N
    ).

% 2. RECOMMANDATION DE MÉTIERS
% Recommande les meilleurs métiers pour un profil donné
recommander_metiers(Personne, MetiersRecommandes) :-
    format('~n=== Recommandation de métiers pour ~w ===~n', [Personne]),
    (findall(metier_score(Metier, Score, Details), (
        metier(Metier, _),
        compatible_avec_metier(Personne, Metier, Score, Details),
        Score > 40  % Seuil minimal de compatibilité
    ), TousMetiers) ->
        % Tri des métiers par score décroissant
        sort(2, @>=, TousMetiers, MetiersTries),
        
        % Limitation aux 5 meilleurs résultats
        (length(MetiersTries, L), L > 5 ->
            prefix(MetiersTries, 5, MetiersRecommandes)
        ;
            MetiersRecommandes = MetiersTries
        ),
        afficher_metiers_recommandes(MetiersRecommandes)
    ;
        format('Aucun métier recommandé trouvé pour ~w.~n', [Personne]),
        MetiersRecommandes = []
    ).

% Afficher les métiers recommandés
afficher_metiers_recommandes(MetiersRecommandes) :-
    format('Métiers recommandés:~n', []),
    forall(member(metier_score(Metier, Score, _), MetiersRecommandes),
        format('  - ~w: Score = ~w/100~n', [Metier, Score])).

% 3. ANALYSE DES ASPIRATIONS PROFESSIONNELLES
% Évalue la compatibilité avec l'aspiration déclarée
evaluer_aspiration(Personne, MetierSouhaite, Evaluation) :-
    format('~n=== Évaluation de l\'aspiration pour ~w ===~n', [MetierSouhaite]),
    (compatible_avec_metier(Personne, MetierSouhaite, Score, Details) ->
        % Interprétation du score
        (Score >= 80 ->
            Realisme = "Très réaliste",
            Message = "Votre profil correspond très bien à ce métier"
        ; Score >= 60 ->
            Realisme = "Réaliste",
            Message = "Votre profil correspond bien à ce métier avec quelques points à développer"
        ; Score >= 40 ->
            Realisme = "Possible avec effort",
            Message = "Ce métier est accessible mais nécessitera des efforts importants"
        ;
            Realisme = "Peu réaliste",
            Message = "Ce métier semble peu adapté à votre profil actuel"
        ),
        
        % Construction de l'évaluation détaillée
        Evaluation = evaluation(
            score(Score),
            realisme(Realisme),
            message(Message),
            details(Details)
        ),
        format('Résultat: ~w (~w)~n', [Realisme, Message])
    ;
        throw(error(evaluation_error, 'Impossible d\'évaluer l\'aspiration'))
    ).

% 4. IDENTIFICATION DES MATIÈRES CLÉS
% Identifie les matières à renforcer pour un métier donné
matieres_a_renforcer(Personne, Metier, MatieresTriees) :-
    format('~n=== Matières à renforcer pour ~w ===~n', [Metier]),
    % Récupération des compétences requises pour le métier
    (findall(matiere_detail(Matiere, NiveauImportance, NiveauActuel, Ecart, Priorite), (
        requiert_competence(Comp, Metier, NiveauRequisComp),
        importance_matiere(Matiere, Comp, NiveauImportance),
        (niveau_matiere(Personne, Matiere, NiveauActuel) ->
            true
        ;
            NiveauActuel = 0
        ),
        Ecart is NiveauImportance - NiveauActuel,
        Ecart > 0,  % Ne garder que les matières avec un écart positif
        % Calcul de la priorité en fonction de l'écart et de l'importance
        Priorite is Ecart * NiveauImportance
    ), Matieres) ->
        % Tri des matières par priorité décroissante
        sort(4, @>=, Matieres, MatieresTriees),
        afficher_matieres_a_renforcer(MatieresTriees)
    ;
        format('Aucune matière à renforcer trouvée pour ~w.~n', [Metier]),
        MatieresTriees = []
    ).

% Afficher les matières à renforcer
afficher_matieres_a_renforcer(MatieresTriees) :-
    format('Matières à renforcer:~n', []),
    forall(member(matiere_detail(Matiere, NiveauImportance, NiveauActuel, Ecart, _), MatieresTriees),
        format('  - ~w: Niveau actuel = ~w, Niveau requis = ~w, Écart = ~w~n', [Matiere, NiveauActuel, NiveauImportance, Ecart])).

% 5. ÉLABORATION D'UN PLAN DE DÉVELOPPEMENT
% Génère un plan de développement personnalisé
generer_plan_developpement(Personne, Metier, Plan) :-
    format('~n=== Plan de développement pour ~w ===~n', [Metier]),
    % Récupération des différentes composantes du plan
    (matieres_a_renforcer(Personne, Metier, MatieresARenforcer) ->
        true
    ;
        MatieresARenforcer = []
    ),
    (niveau_etude(Personne, NiveauEtudeActuel) ->
        true
    ;
        throw(error(missing_data, 'Niveau d\'études non défini pour la personne'))
    ),
    (niveau_etude_requis(Metier, NiveauEtudeRequis) ->
        true
    ;
        throw(error(missing_data, 'Niveau d\'études requis non défini pour le métier'))
    ),
    (ecart_competences(Personne, Metier, EcartCompetences) ->
        true
    ;
        EcartCompetences = []
    ),
    (recommander_formations(Metier, Personne, Formations) ->
        true
    ;
        Formations = []
    ),
    (estimer_duree_preparation(Personne, Metier, DureeEstimee) ->
        true
    ;
        DureeEstimee = 'Non spécifiée'
    ),
    
    % Construction du plan complet
    Plan = plan(
        metier(Metier),
        niveau_actuel(NiveauEtudeActuel),
        niveau_requis(NiveauEtudeRequis),
        ecart_formation(max(0, NiveauEtudeRequis - NiveauEtudeActuel)),
        matieres_prioritaires(MatieresARenforcer),
        competences_a_developper(EcartCompetences),
        formations_recommandees(Formations),
        duree_estimee(DureeEstimee),
        etapes_intermediaires(EtapesIntermediaires)
    ),
    afficher_plan_developpement(Plan).

% Afficher le plan de développement
afficher_plan_developpement(Plan) :-
    Plan = plan(Metier, NiveauActuel, NiveauRequis, EcartFormation, MatieresPrioritaires, CompetencesADevelopper, FormationsRecommandees, DureeEstimee, EtapesIntermediaires),
    format('Métier cible: ~w~n', [Metier]),
    format('Niveau d\'études actuel: ~w (Requis: ~w)~n', [NiveauActuel, NiveauRequis]),
    format('Écart de formation: ~w~n', [EcartFormation]),
    format('Matières prioritaires:~n', []),
    forall(member(Matiere, MatieresPrioritaires), format('  - ~w~n', [Matiere])),
    format('Compétences à développer:~n', []),
    forall(member(Competence, CompetencesADevelopper), format('  - ~w~n', [Competence])),
    format('Formations recommandées:~n', []),
    forall(member(Formation, FormationsRecommandees), format('  - ~w~n', [Formation])),
    format('Durée estimée: ~w années~n', [DureeEstimee]),
    format('Étapes intermédiaires:~n', []),
    forall(member(Etape, EtapesIntermediaires), format('  - ~w~n', [Etape])).