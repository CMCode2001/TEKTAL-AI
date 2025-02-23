% ======= MOTEUR D'INFÉRENCE INTERACTIF CORRIGÉ =======

% 1. Point d'entrée pour interagir avec le moteur d'inférence
demarrer :-
    format('~nBienvenue dans le moteur d\'inférence interactif!~n', []),
    format('Je vais vous poser des questions pour vous aider à explorer les métiers, les compétences, les formations, etc.~n', []),
    format('Pour quitter, tapez "quitter" à tout moment.~n~n', []),
    menu_principal.

% 2. Menu principal interactif
menu_principal :-
    repeat,
    format('~n=== MENU PRINCIPAL ===~n', []),
    format('1. Explorer les métiers~n', []),
    format('2. Évaluer la compatibilité avec un métier~n', []),
    format('3. Recommander des métiers~n', []),
    format('4. Explorer les formations~n', []),
    format('0. Quitter~n', []),
    format('~nVotre choix: ', []),
    read(Choix),
    (Choix = 0 ->
        format('Au revoir!~n', []),
        !
    ;
        traiter_choix(Choix),
        fail
    ).

% 3. Traitement des choix de l'utilisateur
traiter_choix(1) :-
    explorer_metiers.

traiter_choix(2) :-
    evaluer_compatibilite_interactive.

traiter_choix(3) :-
    recommander_metiers_interactive.

traiter_choix(4) :-
    explorer_formations.

traiter_choix(_) :-
    format('Choix invalide. Veuillez réessayer.~n', []).

% 4. Explorer les métiers
explorer_metiers :-
    repeat,
    format('~n=== EXPLORER LES MÉTIERS ===~n', []),
    format('1. Lister tous les métiers~n', []),
    format('2. Lister les métiers dans un domaine spécifique~n', []),
    format('3. Afficher les détails d\'un métier~n', []),
    format('0. Retour au menu principal~n', []),
    format('~nVotre choix: ', []),
    read(Choix),
    (Choix = 0 ->
        menu_principal
    ; Choix = 1 ->
        lister_metiers
    ; Choix = 2 ->
        format('Entrez le domaine: ', []),
        read(Domaine),
        lister_metiers_domaine(Domaine)
    ; Choix = 3 ->
        format('Entrez le métier: ', []),
        read(Metier),
        details_metier(Metier)
    ;
        format('Choix invalide. Veuillez réessayer.~n', [])
    ).

% 5. Évaluer la compatibilité avec un métier (interactif)
evaluer_compatibilite_interactive :-
    format('~n=== ÉVALUER LA COMPATIBILITÉ AVEC UN MÉTIER ===~n', []),
    format('Entrez votre nom: ', []),
    read(Personne),
    format('Entrez le métier que vous souhaitez évaluer: ', []),
    read(Metier),
    evaluer_compatibilite(Personne, Metier).

% 6. Recommander des métiers (interactif)
recommander_metiers_interactive :-
    format('~n=== RECOMMANDER DES MÉTIERS ===~n', []),
    format('Entrez votre nom: ', []),
    read(Personne),
    recommander_metiers(Personne).

% 7. Explorer les formations
explorer_formations :-
    repeat,
    format('~n=== EXPLORER LES FORMATIONS ===~n', []),
    format('1. Lister toutes les écoles~n', []),
    format('2. Lister les formations disponibles dans une école~n', []),
    format('3. Afficher les détails d\'une formation~n', []),
    format('0. Retour au menu principal~n', []),
    format('~nVotre choix: ', []),
    read(Choix),
    (Choix = 0 ->
        menu_principal
    ; Choix = 1 ->
        lister_ecoles
    ; Choix = 2 ->
        format('Entrez le nom de l\'école: ', []),
        read(Ecole),
        formations_ecole(Ecole)
    ; Choix = 3 ->
        format('Entrez le nom de l\'école: ', []),
        read(Ecole),
        format('Entrez le nom de la formation: ', []),
        read(Formation),
        details_formation(Ecole, Formation)
    ;
        format('Choix invalide. Veuillez réessayer.~n', [])
    ).

% 8. Lister tous les métiers disponibles
lister_metiers :-
    format('~n=== Liste des métiers disponibles ===~n', []),
    forall(metier(Metier, Domaine),
        format('  - ~w (Domaine: ~w)~n', [Metier, Domaine])).

% 9. Lister les métiers dans un domaine spécifique
lister_metiers_domaine(Domaine) :-
    format('~n=== Métiers dans le domaine ~w ===~n', [Domaine]),
    (metier(Metier, Domaine) ->
        forall(metier(Metier, Domaine),
            format('  - ~w~n', [Metier]))
    ;
        format('Aucun métier trouvé dans le domaine ~w.~n', [Domaine])
    ).

% 10. Afficher les détails d'un métier spécifique
details_metier(Metier) :-
    format('~n=== Détails du métier ~w ===~n', [Metier]),
    (metier(Metier, Domaine) ->
        format('Domaine: ~w~n', [Domaine]),
        (description(Metier, Description) ->
            format('Description: ~w~n', [Description])
        ;
            format('Description: Non disponible~n', [])
        )),
        (niveau_etude_requis(Metier, Niveau) ->
            format('Niveau d\'études requis: ~w~n', [Niveau])
        ;
            format('Niveau d\'études requis: Non spécifié~n', [])
        ),
        (salaire_moyen(Metier, Salaire) ->
            format('Salaire moyen: ~w k€/an~n', [Salaire])
        ;
            format('Salaire moyen: Non spécifié~n', [])
        ),
        format('Compétences requises:~n', []),
        forall(requiert_competence(Comp, Metier, NiveauRequis),
            format('  - ~w (Niveau requis: ~w)~n', [Comp, NiveauRequis])
    ;
        format('Le métier ~w n\'existe pas dans la base de connaissances.~n', [Metier])
    ).

% 11. Évaluer la compatibilité d'une personne avec un métier
evaluer_compatibilite(Personne, Metier) :-
    format('~n=== Évaluation de la compatibilité de ~w avec ~w ===~n', [Personne, Metier]),
    (compatible_avec_metier(Personne, Metier, Score, Details) ->
        format('Score de compatibilité: ~w/100~n', [Score]),
        Details = details(Etude, Competences, Interet, Personnalite),
        format('Détails:~n', []),
        format('  - Niveau d\'études: ~w~n', [Etude]),
        format('  - Compétences: ~w~n', [Competences]),
        format('  - Intérêt: ~w~n', [Interet]),
        format('  - Personnalité: ~w~n', [Personnalite])
    ;
        format('Impossible d\'évaluer la compatibilité pour ~w avec ~w.~n', [Personne, Metier])
    ).

% 12. Recommander des métiers pour une personne
recommander_metiers(Personne) :-
    format('~n=== Recommandation de métiers pour ~w ===~n', [Personne]),
    (findall(Metier-Score, (
        metier(Metier, _),
        compatible_avec_metier(Personne, Metier, Score, _),
        Score > 40  % Seuil minimal de compatibilité
    ), Metiers) ->
        (Metiers = [] ->
            format('Aucun métier recommandé pour ~w.~n', [Personne])
        ;
            sort(2, @>=, Metiers, MetiersTries),
            forall(member(Metier-Score, MetiersTries),
                format('  - ~w (Score: ~w/100)~n', [Metier, Score]))
    ;
        format('Impossible de recommander des métiers pour ~w.~n', [Personne])
    )).

% 13. Lister toutes les écoles disponibles
lister_ecoles :-
    format('~n=== Liste des écoles disponibles ===~n', []),
    forall(ecole(Ecole, Domaine, Localisation),
        format('  - ~w (Domaine: ~w, Localisation: ~w)~n', [Ecole, Domaine, Localisation])).

% 14. Lister les formations disponibles dans une école spécifique
formations_ecole(Ecole) :-
    format('~n=== Formations disponibles à ~w ===~n', [Ecole]),
    (findall(Formation, formation(Ecole, _, Formation), Formations) ->
        (Formations = [] ->
            format('Aucune formation disponible à ~w.~n', [Ecole])
        ;
            forall(member(Formation, Formations),
                format('  - ~w~n', [Formation]))
    ;
        format('L\'école ~w n\'existe pas dans la base de connaissances.~n', [Ecole])
    )).

% 15. Afficher les détails d'une formation spécifique
details_formation(Ecole, Formation) :-
    format('~n=== Détails de la formation ~w à ~w ===~n', [Formation, Ecole]),
    (formation(Ecole, Niveau, Formation) ->
        format('Niveau: ~w~n', [Niveau]),
        (cout_formation(Ecole, Formation, Cout) ->
            format('Coût: ~w€~n', [Cout])
        ;
            format('Coût: Non spécifié~n', [])
        ),
        (duree_formation(Ecole, Formation, Duree) ->
            format('Durée: ~w années~n', [Duree])
        ;
            format('Durée: Non spécifiée~n', [])
        )
    ;
        format('La formation ~w n\'existe pas à ~w.~n', [Formation, Ecole])
    ).