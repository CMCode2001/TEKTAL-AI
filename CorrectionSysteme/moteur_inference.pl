:- consult('base_de_connaissance.pl').

% Vérifier si une filière permet d'accéder à un domaine donné
filiere_autorisee(Niveau, Filiere, Domaine) :-
    correspondance_filiere_domaine(Niveau, Filiere, Domaine).

% Trouver les domaines possibles pour une filière donnée
domaines_possibles(Niveau, Filiere, Domaines) :-
    findall(Domaine, correspondance_filiere_domaine(Niveau, Filiere, Domaine), Domaines).

% Afficher les matières requises si la filière est valide
proposer_matieres(Domaine, Niveau, Filiere) :-
    matiere_requise(Domaine, Niveau, Filiere, Matieres),
    write("Pour le domaine "), write(Domaine), write(" au niveau "), write(Niveau),
    write(" (Filière : "), write(Filiere), write("), les matières requises sont : "), nl,
    write(Matieres), nl,
    write("Veuillez entrer vos notes sous forme [matiere1-note, matiere2-note, ...]."), nl.

% Vérifier si l'utilisateur peut s'orienter vers un domaine donné
peut_sorienter(Domaine, Niveau, Filiere, Notes) :-
    matiere_requise(Domaine, Niveau, Filiere, Matieres),
    verifier_notes(Matieres, Notes).

% Vérification des notes minimales
verifier_notes([], _).
verifier_notes([M|R], Notes) :-
    member(M-Note, Notes),
    Note >= 10,  % Note minimale requise
    verifier_notes(R, Notes).

% Fonction principale pour orienter l'utilisateur
orienter_utilisateur(Nom, Niveau, Filiere, Domaine) :-
    (   filiere_autorisee(Niveau, Filiere, Domaine) ->
        proposer_matieres(Domaine, Niveau, Filiere),  % Étape 1 : Vérification de la filière et affichage des matières
        read(Notes),  % Étape 2 : Lire les notes entrées par l'utilisateur
        (   peut_sorienter(Domaine, Niveau, Filiere, Notes) ->
            etablissement(Domaine, Etablissements),
            parcours(Domaine, Niveau, Parcours),
            write(Nom), write(", vous pouvez poursuivre en " ), write(Domaine), nl,
            write("Les établissements recommandés sont : "), write(Etablissements), nl,
            write("Les parcours possibles pour réussir dans ce domaine sont : "), write(Parcours), nl
        ;   write(Nom), write(", désolé, vous ne remplissez pas les conditions pour " ), write(Domaine), nl
        )
    ;   % Si la filière ne correspond pas, proposer des alternatives
        write(Nom), write(", votre filière ("), write(Filiere), write(") ne permet pas d’accéder à "),
        write(Domaine), write("."), nl,
        domaines_possibles(Niveau, Filiere, Domaines),
        (   Domaines \= [] ->
            write("Cependant, avec votre filière, vous pouvez vous orienter vers : "), nl,
            write(Domaines), nl,
            write("Veuillez entrer un nouveau domaine parmi ceux proposés : "), nl,
            read(NouveauDomaine),
            orienter_utilisateur(Nom, Niveau, Filiere, NouveauDomaine)
        ;   write("Malheureusement, aucun domaine n’est disponible pour votre filière."), nl
        )
    ).

% Fonction pour explorer les domaines disponibles
explorer_domaines :-
    write("===== Exploration des Domaines Professionnels ====="), nl, nl,
    findall(Domaine, matiere_requise(Domaine, _, _), DomainesUniques),
    list_to_set(DomainesUniques, Domaines), % Éliminer les doublons
    afficher_domaines(Domaines).

% Afficher les informations pour chaque domaine
afficher_domaines([]).
afficher_domaines([Domaine|Rest]) :-
    write(" Domaine : "), write(Domaine), nl,
    findall(Niveau-Filiere, correspondance_filiere_domaine(Niveau, Filiere, Domaine), Filieres),
    findall(Matieres, matiere_requise(Domaine, _, Matieres), MatieresList),
    list_to_set(MatieresList, MatieresUnique),
    write("   Matières requises : "), write(MatieresUnique), nl,
    write("   Filières compatibles : "), write(Filieres), nl, nl,
    afficher_domaines(Rest).

% % Fonction principale pour recommander des domaines basés sur la fréquence d'apparition (3 fois ou plus)
% recommander_domaines_pertinents(Niveau, Filiere, CompetencesTech, Personnalites) :-

%     % Trouver les domaines liés à la filière de l'utilisateur
%     findall(DomaineFiliere, correspondance_filiere_domaine(Niveau, Filiere, DomaineFiliere), DomainesFiliere),

%     % Trouver les domaines liés aux compétences techniques
%     trouver_domaines_par_competences(CompetencesTech, DomainesCompetences),

%     % Trouver les domaines liés aux critères de personnalité
%     trouver_domaines_par_personnalite(Personnalites, DomainesPerso),

%     % Fusionner toutes les sources
%     append([DomainesFiliere, DomainesCompetences, DomainesPerso], TousDomaines),

%     % Compter l'occurrence de chaque domaine
%     compter_occurrences(TousDomaines, Comptes),

%     % Filtrer uniquement les domaines qui apparaissent au moins 3 fois
%     findall(Domaine, (member(Domaine-Occ, Comptes), Occ >= 3), DomainesPertinents),

%     % Afficher les résultats
%     (DomainesPertinents == [] -> 
%         write(" Aucun domaine trouvé correspondant à votre profil."), nl 
%     ; 
%         write(" Voici les domaines les plus adaptés à votre profil :"), nl,
%         afficher_liste(DomainesPertinents)
%     ).

% Fonction principale pour recommander des domaines basés sur la fréquence d'apparition (3 fois ou plus)
recommander_domaines_pertinents(Niveau, Filiere, CompetencesTech, Personnalites, DomainesPertinents) :-

    % Trouver les domaines liés à la filière de l'utilisateur
    findall(DomaineFiliere, correspondance_filiere_domaine(Niveau, Filiere, DomaineFiliere), DomainesFiliere),

    % Trouver les domaines liés aux compétences techniques
    trouver_domaines_par_competences(CompetencesTech, DomainesCompetences),

    % Trouver les domaines liés aux critères de personnalité
    trouver_domaines_par_personnalite(Personnalites, DomainesPerso),

    % Fusionner toutes les sources
    append([DomainesFiliere, DomainesCompetences, DomainesPerso], TousDomaines),

    % Compter l'occurrence de chaque domaine
    compter_occurrences(TousDomaines, Comptes),

    % Filtrer uniquement les domaines qui apparaissent au moins 3 fois
    findall(Domaine, (member(Domaine-Occ, Comptes), Occ >= 3), DomainesPertinents).


% Trouver les domaines correspondant aux compétences techniques
trouver_domaines_par_competences([], []).
trouver_domaines_par_competences([C|Rest], Domaines) :-
    competence_technique_domaine(C, DomaineC),
    trouver_domaines_par_competences(Rest, DRest),
    append(DomaineC, DRest, Domaines).

% Trouver les domaines correspondant aux critères de personnalité
trouver_domaines_par_personnalite([], []).
trouver_domaines_par_personnalite([P|Rest], Domaines) :-
    personnalite_domaine(P, DomaineP),
    trouver_domaines_par_personnalite(Rest, DRest),
    append(DomaineP, DRest, Domaines).

% Fonction pour compter l'occurrence de chaque domaine
compter_occurrences(Liste, Comptes) :-
    msort(Liste, Triee), % Trie la liste pour grouper les éléments identiques
    compter_occurrences_triees(Triee, Comptes).

compter_occurrences_triees([], []).
compter_occurrences_triees([X|Xs], [X-N|Comptes]) :-
    occurrences_de(X, [X|Xs], N, Rest),
    compter_occurrences_triees(Rest, Comptes).

% Fonction pour compter combien de fois un élément X apparaît dans une liste triée
occurrences_de(_, [], 0, []).
occurrences_de(X, [X|Xs], N, Rest) :- occurrences_de(X, Xs, N1, Rest), N is N1 + 1.
occurrences_de(X, [Y|Ys], 0, [Y|Ys]) :- X \= Y.

% Fonction pour afficher une liste proprement
afficher_liste([]).
afficher_liste([X|Rest]) :-
    write("  - "), write(X), nl,
    afficher_liste(Rest).
