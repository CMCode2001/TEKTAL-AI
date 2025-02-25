% ======= MOTEUR D'INFÉRENCE AVEC ENREGISTREMENT PERMANENT DES DONNÉES UTILISATEUR =======

% 1. Déclarer les prédicats dynamiques pour stocker les informations de l'utilisateur
:- dynamic niveau_etude/2.
:- dynamic niveau_competence/3.
:- dynamic niveau_matiere/3.
:- dynamic interet/3.
:- dynamic trait_personnalite/3.
:- dynamic profile_exists/1.

% 2. Point d'entrée pour interagir avec le moteur d'inférence
demarrer :-
    format('~nBienvenue dans le moteur d\'inférence interactif!~n', []),
    format('Je vais vous poser des questions pour vous aider à explorer les métiers, les compétences, les formations, etc.~n', []),
    format('Pour quitter, tapez "quitter" à tout moment.~n~n', []),
    menu_principal.

% 3. Menu principal interactif
menu_principal :-
    repeat,
    format('~n=== MENU PRINCIPAL ===~n', []),
    format('1. Explorer les métiers~n', []),
    format('2. Évaluer la compatibilité avec un métier~n', []),
    format('3. Recommander des métiers~n', []),
    format('4. Explorer les formations~n', []),
    format('5. Gérer votre profil~n', []),
    format('6. Sauvegarder les données~n', []),
    format('7. Charger les données~n', []),
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

% 4. Traitement des choix de l'utilisateur
traiter_choix(1) :-
    explorer_metiers.

traiter_choix(2) :-
    evaluer_compatibilite_interactive.

traiter_choix(3) :-
    recommander_metiers_interactive.

traiter_choix(4) :-
    explorer_formations.

traiter_choix(5) :-
    gerer_profil.

traiter_choix(6) :-
    sauvegarder_donnees.

traiter_choix(7) :-
    charger_donnees.

traiter_choix(_) :-
    format('Choix invalide. Veuillez réessayer.~n', []).

% 5. Gérer le profil utilisateur
gerer_profil :-
    repeat,
    format('~n=== GÉRER VOTRE PROFIL ===~n', []),
    format('1. Créer un nouveau profil~n', []),
    format('2. Modifier un profil existant~n', []),
    format('3. Afficher un profil existant~n', []),
    format('4. Supprimer un profil~n', []),
    format('0. Retour au menu principal~n', []),
    format('~nVotre choix: ', []),
    read(Choix),
    (Choix = 0 ->
        !
    ; Choix = 1 ->
        creer_nouveau_profil
    ; Choix = 2 ->
        modifier_profil
    ; Choix = 3 ->
        afficher_profil
    ; Choix = 4 ->
        supprimer_profil
    ;
        format('Choix invalide. Veuillez réessayer.~n', [])
    ).

% 6. Créer un nouveau profil
creer_nouveau_profil :-
    format('~n=== CRÉER UN NOUVEAU PROFIL ===~n', []),
    format('Entrez votre nom: ', []),
    read(Nom),
    (profile_exists(Nom) ->
        format('Un profil pour ~w existe déjà. Veuillez utiliser l\'option de modification.~n', [Nom])
    ;
        retractall(niveau_etude(Nom, _)),
        retractall(niveau_competence(Nom, _, _)),
        retractall(niveau_matiere(Nom, _, _)),
        retractall(interet(Nom, _, _)),
        retractall(trait_personnalite(Nom, _, _)),
        
        format('Entrez votre niveau d\'études (1=bac, 2=bac+2, 3=licence, 4=master, 5=doctorat): ', []),
        read(NiveauEtude),
        assertz(niveau_etude(Nom, NiveauEtude)),
        assertz(profile_exists(Nom)),
        
        % Saisie des compétences
        format('~nEntrez vos compétences (format: competence-niveau, exemple: programmation-4)~n', []),
        format('Compétences disponibles: programmation, analyse_donnees, securite_informatique, sciences, communication, negociation, gestion_projet, mathematiques, physique, sciences_humaines, biologie, langues~n', []),
        format('Tapez "fin" pour terminer.~n', []),
        saisir_competences(Nom),
        
        % Saisie des matières
        format('~nEntrez vos niveaux de matières (format: matiere-niveau, exemple: informatique-4)~n', []),
        format('Matières disponibles: informatique, statistiques, mathematiques, physique, biologie, chimie, francais, langues_etrangeres, economie, management, psychologie, sociologie~n', []),
        format('Tapez "fin" pour terminer.~n', []),
        saisir_matieres(Nom),
        
        % Saisie des intérêts
        format('~nEntrez vos intérêts (format: domaine-niveau, exemple: informatique-5)~n', []),
        format('Domaines disponibles: informatique, sante, commerce, ingenierie, sciences_humaines~n', []),
        format('Tapez "fin" pour terminer.~n', []),
        saisir_interets(Nom),
        
        % Saisie des traits de personnalité
        format('~nEntrez vos traits de personnalité (format: trait-niveau, exemple: creativite-4)~n', []),
        format('Traits disponibles: creativite, rigueur, empathie, patience, persuasion, leadership, precision, logique, ecoute~n', []),
        format('Tapez "fin" pour terminer.~n', []),
        saisir_traits_personnalite(Nom),
        
        format('Profil de ~w créé avec succès.~n', [Nom])
    ).

% 7. Modifier un profil existant
modifier_profil :-
    format('~n=== MODIFIER UN PROFIL EXISTANT ===~n', []),
    format('Entrez votre nom: ', []),
    read(Nom),
    (profile_exists(Nom) ->
        repeat,
        format('~nQue souhaitez-vous modifier?~n', []),
        format('1. Niveau d\'études~n', []),
        format('2. Compétences~n', []),
        format('3. Matières~n', []),
        format('4. Intérêts~n', []),
        format('5. Traits de personnalité~n', []),
        format('0. Terminer les modifications~n', []),
        format('~nVotre choix: ', []),
        read(ChoixModif),
        (ChoixModif = 0 ->
            !
        ; ChoixModif = 1 ->
            format('Entrez votre nouveau niveau d\'études: ', []),
            read(NouveauNiveau),
            retractall(niveau_etude(Nom, _)),
            assertz(niveau_etude(Nom, NouveauNiveau)),
            format('Niveau d\'études mis à jour.~n', [])
        ; ChoixModif = 2 ->
            format('Souhaitez-vous ajouter, modifier ou supprimer des compétences? (ajouter/modifier/supprimer) ', []),
            read(ActionComp),
            modifier_competences(Nom, ActionComp)
        ; ChoixModif = 3 ->
            format('Souhaitez-vous ajouter, modifier ou supprimer des matières? (ajouter/modifier/supprimer) ', []),
            read(ActionMat),
            modifier_matieres(Nom, ActionMat)
        ; ChoixModif = 4 ->
            format('Souhaitez-vous ajouter, modifier ou supprimer des intérêts? (ajouter/modifier/supprimer) ', []),
            read(ActionInt),
            modifier_interets(Nom, ActionInt)
        ; ChoixModif = 5 ->
            format('Souhaitez-vous ajouter, modifier ou supprimer des traits de personnalité? (ajouter/modifier/supprimer) ', []),
            read(ActionTrait),
            modifier_traits_personnalite(Nom, ActionTrait)
        ;
            format('Choix invalide. Veuillez réessayer.~n', [])
        ),
        ChoixModif \= 0
    ;
        format('Aucun profil trouvé pour ~w. Veuillez d\'abord créer un profil.~n', [Nom])
    ).

% 8. Modifier les compétences
modifier_competences(Nom, ajouter) :-
    format('Entrez les nouvelles compétences (format: competence-niveau):~n', []),
    format('Tapez "fin" pour terminer.~n', []),
    saisir_competences(Nom).

modifier_competences(Nom, modifier) :-
    format('Quelle compétence souhaitez-vous modifier? ', []),
    read(Comp),
    (niveau_competence(Nom, Comp, _) ->
        format('Entrez le nouveau niveau: ', []),
        read(NouveauNiveau),
        retractall(niveau_competence(Nom, Comp, _)),
        assertz(niveau_competence(Nom, Comp, NouveauNiveau)),
        format('Compétence ~w mise à jour.~n', [Comp])
    ;
        format('Compétence ~w non trouvée pour ~w.~n', [Comp, Nom])
    ).

modifier_competences(Nom, supprimer) :-
    format('Quelle compétence souhaitez-vous supprimer? ', []),
    read(Comp),
    (retract(niveau_competence(Nom, Comp, _)) ->
        format('Compétence ~w supprimée.~n', [Comp])
    ;
        format('Compétence ~w non trouvée pour ~w.~n', [Comp, Nom])
    ).

% 9. Modifier les matières
modifier_matieres(Nom, ajouter) :-
    format('Entrez les nouvelles matières (format: matiere-niveau):~n', []),
    format('Tapez "fin" pour terminer.~n', []),
    saisir_matieres(Nom).

modifier_matieres(Nom, modifier) :-
    format('Quelle matière souhaitez-vous modifier? ', []),
    read(Mat),
    (niveau_matiere(Nom, Mat, _) ->
        format('Entrez le nouveau niveau: ', []),
        read(NouveauNiveau),
        retractall(niveau_matiere(Nom, Mat, _)),
        assertz(niveau_matiere(Nom, Mat, NouveauNiveau)),
        format('Matière ~w mise à jour.~n', [Mat])
    ;
        format('Matière ~w non trouvée pour ~w.~n', [Mat, Nom])
    ).

modifier_matieres(Nom, supprimer) :-
    format('Quelle matière souhaitez-vous supprimer? ', []),
    read(Mat),
    (retract(niveau_matiere(Nom, Mat, _)) ->
        format('Matière ~w supprimée.~n', [Mat])
    ;
        format('Matière ~w non trouvée pour ~w.~n', [Mat, Nom])
    ).

% 10. Modifier les intérêts
modifier_interets(Nom, ajouter) :-
    format('Entrez les nouveaux intérêts (format: domaine-niveau):~n', []),
    format('Tapez "fin" pour terminer.~n', []),
    saisir_interets(Nom).

modifier_interets(Nom, modifier) :-
    format('Quel intérêt souhaitez-vous modifier? ', []),
    read(Int),
    (interet(Nom, Int, _) ->
        format('Entrez le nouveau niveau: ', []),
        read(NouveauNiveau),
        retractall(interet(Nom, Int, _)),
        assertz(interet(Nom, Int, NouveauNiveau)),
        format('Intérêt ~w mis à jour.~n', [Int])
    ;
        format('Intérêt ~w non trouvé pour ~w.~n', [Int, Nom])
    ).

modifier_interets(Nom, supprimer) :-
    format('Quel intérêt souhaitez-vous supprimer? ', []),
    read(Int),
    (retract(interet(Nom, Int, _)) ->
        format('Intérêt ~w supprimé.~n', [Int])
    ;
        format('Intérêt ~w non trouvé pour ~w.~n', [Int, Nom])
    ).

% 11. Modifier les traits de personnalité
modifier_traits_personnalite(Nom, ajouter) :-
    format('Entrez les nouveaux traits de personnalité (format: trait-niveau):~n', []),
    format('Tapez "fin" pour terminer.~n', []),
    saisir_traits_personnalite(Nom).

modifier_traits_personnalite(Nom, modifier) :-
    format('Quel trait souhaitez-vous modifier? ', []),
    read(Trait),
    (trait_personnalite(Nom, Trait, _) ->
        format('Entrez le nouveau niveau: ', []),
        read(NouveauNiveau),
        retractall(trait_personnalite(Nom, Trait, _)),
        assertz(trait_personnalite(Nom, Trait, NouveauNiveau)),
        format('Trait ~w mis à jour.~n', [Trait])
    ;
        format('Trait ~w non trouvé pour ~w.~n', [Trait, Nom])
    ).

modifier_traits_personnalite(Nom, supprimer) :-
    format('Quel trait souhaitez-vous supprimer? ', []),
    read(Trait),
    (retract(trait_personnalite(Nom, Trait, _)) ->
        format('Trait ~w supprimé.~n', [Trait])
    ;
        format('Trait ~w non trouvé pour ~w.~n', [Trait, Nom])
    ).

% 12. Afficher un profil existant
afficher_profil :-
    format('~n=== AFFICHER UN PROFIL EXISTANT ===~n', []),
    format('Entrez le nom: ', []),
    read(Nom),
    (profile_exists(Nom) ->
        format('~n=== Profil de ~w ===~n', [Nom]),
        % Afficher le niveau d'études
        (niveau_etude(Nom, Niveau) ->
            format('Niveau d\'études: ~w~n', [Niveau])
        ;
            format('Niveau d\'études: Non spécifié~n', [])
        ),
        
        % Afficher les compétences
        format('~nCompétences:~n', []),
        (findall(Comp-Niv, niveau_competence(Nom, Comp, Niv), Competences) ->
            (Competences = [] ->
                format('  Aucune compétence enregistrée~n', [])
            ;
                forall(member(C-N, Competences), format('  - ~w: ~w~n', [C, N]))
            )
        ;
            format('  Aucune compétence enregistrée~n', [])
        ),
        
        % Afficher les matières
        format('~nMatières:~n', []),
        (findall(Mat-Niv, niveau_matiere(Nom, Mat, Niv), Matieres) ->
            (Matieres = [] ->
                format('  Aucune matière enregistrée~n', [])
            ;
                forall(member(M-N, Matieres), format('  - ~w: ~w~n', [M, N]))
            )
        ;
            format('  Aucune matière enregistrée~n', [])
        ),
        
        % Afficher les intérêts
        format('~nIntérêts:~n', []),
        (findall(Int-Niv, interet(Nom, Int, Niv), Interets) ->
            (Interets = [] ->
                format('  Aucun intérêt enregistré~n', [])
            ;
                forall(member(I-N, Interets), format('  - ~w: ~w~n', [I, N]))
            )
        ;
            format('  Aucun intérêt enregistré~n', [])
        ),
        
        % Afficher les traits de personnalité
        format('~nTraits de personnalité:~n', []),
        (findall(Trait-Niv, trait_personnalite(Nom, Trait, Niv), Traits) ->
            (Traits = [] ->
                format('  Aucun trait de personnalité enregistré~n', [])
            ;
                forall(member(T-N, Traits), format('  - ~w: ~w~n', [T, N]))
            )
        ;
            format('  Aucun trait de personnalité enregistré~n', [])
        )
    ;
        format('Aucun profil trouvé pour ~w.~n', [Nom])
    ).

% 13. Supprimer un profil
supprimer_profil :-
    format('~n=== SUPPRIMER UN PROFIL ===~n', []),
    format('Entrez le nom: ', []),
    read(Nom),
    (profile_exists(Nom) ->
        format('Êtes-vous sûr de vouloir supprimer le profil de ~w? (oui/non) ', [Nom]),
        read(Confirmation),
        (Confirmation = oui ->
            retractall(niveau_etude(Nom, _)),
            retractall(niveau_competence(Nom, _, _)),
            retractall(niveau_matiere(Nom, _, _)),
            retractall(interet(Nom, _, _)),
            retractall(trait_personnalite(Nom, _, _)),
            retractall(profile_exists(Nom)),
            format('Profil de ~w supprimé avec succès.~n', [Nom])
        ;
            format('Suppression annulée.~n', [])
        )
    ;
        format('Aucun profil trouvé pour ~w.~n', [Nom])
    ).

% 14. Saisir des compétences
saisir_competences(Nom) :-
    repeat,
    format('> ', []),
    read_line_to_codes(user_input, Codes),
    string_codes(CompetenceNiveau, Codes),
    (CompetenceNiveau = "fin" ->
        !
    ;
        split_string(CompetenceNiveau, "-", "", [CompStr, NiveauStr]),
        atom_string(Competence, CompStr),
        number_string(Niveau, NiveauStr),
        assertz(niveau_competence(Nom, Competence, Niveau)),
        fail
    ).

% 15. Saisir des matières
saisir_matieres(Nom) :-
    repeat,
    format('> ', []),
    read_line_to_codes(user_input, Codes),
    string_codes(MatiereNiveau, Codes),
    (MatiereNiveau = "fin" ->
        !
    ;
        split_string(MatiereNiveau, "-", "", [MatStr, NiveauStr]),
        atom_string(Matiere, MatStr),
        number_string(Niveau, NiveauStr),
        assertz(niveau_matiere(Nom, Matiere, Niveau)),
        fail
    ).

% 16. Saisir des intérêts
saisir_interets(Nom) :-
    repeat,
    format('> ', []),
    read_line_to_codes(user_input, Codes),
    string_codes(DomaineNiveau, Codes),
    (DomaineNiveau = "fin" ->
        !
    ;
        split_string(DomaineNiveau, "-", "", [DomStr, NiveauStr]),
        atom_string(Domaine, DomStr),
        number_string(Niveau, NiveauStr),
        assertz(interet(Nom, Domaine, Niveau)),
        fail
    ).

% 17. Saisir des traits de personnalité
saisir_traits_personnalite(Nom) :-
    repeat,
    format('> ', []),
    read_line_to_codes(user_input, Codes),
    string_codes(TraitNiveau, Codes),
    (TraitNiveau = "fin" ->
        !
    ;
        split_string(TraitNiveau, "-", "", [TraitStr, NiveauStr]),
        atom_string(Trait, TraitStr),
        number_string(Niveau, NiveauStr),
        assertz(trait_personnalite(Nom, Trait, Niveau)),
        fail
    ).

% 18. Sauvegarder les données dans un fichier
sauvegarder_donnees :-
    format('~n=== SAUVEGARDER LES DONNÉES ===~n', []),
    format('Entrez le nom du fichier de sauvegarde: ', []),
    read(Fichier),
    open(Fichier, write, Stream),
    
    % Sauvegarde des profils existants
    forall(profile_exists(Nom), (
        format(Stream, 'profile_exists(~q).~n', [Nom])
    )),
    
    % Sauvegarde des niveaux d'études
    forall(niveau_etude(Nom, Niveau), (
        format(Stream, 'niveau_etude(~q, ~q).~n', [Nom, Niveau])
    )),
    
    % Sauvegarde des compétences
    forall(niveau_competence(Nom, Comp, Niveau), (
        format(Stream, 'niveau_competence(~q, ~q, ~q).~n', [Nom, Comp, Niveau])
    )),
    
    % Sauvegarde des matières
    forall(niveau_matiere(Nom, Mat, Niveau), (
        format(Stream, 'niveau_matiere(~q, ~q, ~q).~n', [Nom, Mat, Niveau])
    )),
    
    % Sauvegarde des intérêts
    forall(interet(Nom, Dom, Niveau), (
        format(Stream, 'interet(~q, ~q, ~q).~n', [Nom, Dom, Niveau])
    )),
    
    % Sauvegarde des traits de personnalité
    forall(trait_personnalite(Nom, Trait, Niveau), (
        format(Stream, 'trait_personnalite(~q, ~q, ~q).~n', [Nom, Trait, Niveau])
    )),
    
    close(Stream),
    format('Données sauvegardées avec succès dans ~w.~n', [Fichier]).

% 19. Charger les données depuis un fichier
charger_donnees :-
    format('~n=== CHARGER LES DONNÉES ===~n', []),
    format('Entrez le nom du fichier à charger: ', []),
    read(Fichier),
    (exists_file(Fichier) ->
        format('Êtes-vous sûr de vouloir charger les données? Cela remplacera les données actuelles. (oui/non) ', []),
        read(Confirmation),
        (Confirmation = oui ->
            % Suppression des données actuelles
            retractall(profile_exists(_)),
            retractall(niveau_etude(_, _)),
            retractall(niveau_competence(_, _, _)),
            retractall(niveau_matiere(_, _, _)),
            retractall(interet(_, _, _)),
            retractall(trait_personnalite(_, _, _)),
            
            % Chargement des données depuis le fichier
            consult(Fichier),
            format('Données chargées avec succès depuis ~w.~n', [Fichier])
        ;
            format('Chargement annulé.~n', [])
        )
    ;
        format('Le fichier ~w n\'existe pas.~n', [Fichier])
    ).

% 20. Explorer les métiers
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
        !
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

% 21. Évaluer la compatibilité avec un métier (interactif)
evaluer_compatibilite_interactive :-
    format('~n=== ÉVALUER LA COMPATIBILITÉ AVEC UN MÉTIER ===~n', []),
    format('Entrez votre nom: ', []),
    read(Personne),
    (profile_exists(Personne) ->
        format('Entrez le métier que vous souhaitez évaluer: ', []),
        read(Metier),
        compatible_avec_metier(Personne, Metier, Score, Details),
        format('Score de compatibilité: ~2f/100~n', [Score]),
        afficher_details_compatibilite(Details)
    ;
        format('Aucun profil trouvé pour ~w. Veuillez d\'abord créer un profil.~n', [Personne])
    ).

% 22. Afficher les détails de compatibilité
afficher_details_compatibilite(Details) :-
    Details = details(
        etude(ScoreEtude, NiveauEtude, NiveauRequisEtude),
        competences(ScoreCompetences, CompDetails),
        interet(ScoreInteret),
        personnalite(ScorePersonnalite)
    ),
    format('Détails:~n', []),
    format('- Niveau d\'études: ~w/100 (Actuel: ~w, Requis: ~w)~n', [ScoreEtude, NiveauEtude, NiveauRequisEtude]),
    format('- Compétences: ~w/100~n', [ScoreCompetences]),
    forall(member(detail(Comp, NiveauRequis, NiveauPersonne, ScoreComp), CompDetails),
        format('  * ~w: Niveau actuel = ~w, Niveau requis = ~w, Score = ~w/100~n', [Comp, NiveauPersonne, NiveauRequis, ScoreComp])),
    format('- Intérêt: ~w/100~n', [ScoreInteret]),
    format('- Personnalité: ~w/100~n', [ScorePersonnalite]).

% 23. Recommander des métiers (interactif)
recommander_metiers_interactive :-
    format('~n=== RECOMMANDER DES MÉTIERS ===~n', []),
    format('Entrez votre nom: ', []),
    read(Personne),
    (profile_exists(Personne) ->
        recommander_metiers(Personne, MetiersRecommandes),
        (MetiersRecommandes = [] ->
            format('Aucun métier recommandé pour ~w.~n', [Personne])
        ;
            format('Métiers recommandés pour ~w:~n', [Personne]),
            forall(member(metier_score(Metier, Score, _), MetiersRecommandes),
                format('  - ~w (Score: ~2f/100)~n', [Metier, Score]))
        )
    ;
        format('Aucun profil trouvé pour ~w. Veuillez d\'abord créer un profil.~n', [Personne])
    ).

% 24. Explorer les formations
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
        !
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

% 25. Lister toutes les écoles disponibles
lister_ecoles :-
format('~n=== Liste des écoles disponibles ===~n', []),
forall(ecole(Ecole, Domaine, Localisation),
    format('  - ~w (Domaine: ~w, Localisation: ~w)~n', [Ecole, Domaine, Localisation])).

% 26. Lister les formations disponibles dans une école spécifique
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

% 27. Afficher les détails d'une formation spécifique
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

% 28. Compatibilité avec un métier
compatible_avec_metier(Personne, Metier, Score, Details) :-
% Vérifier le niveau d'études
niveau_etude(Personne, NiveauEtude),
niveau_etude_requis(Metier, NiveauRequisEtude),
ScoreEtude is (NiveauEtude >= NiveauRequisEtude ? 100 : (NiveauEtude / NiveauRequisEtude) * 100),

% Vérifier les compétences
findall(detail(Comp, NiveauRequis, NiveauPersonne, ScoreComp),
    (requiert_competence(Comp, Metier, NiveauRequis),
     niveau_competence(Personne, Comp, NiveauPersonne),
     ScoreComp is (NiveauPersonne >= NiveauRequis ? 100 : (NiveauPersonne / NiveauRequis) * 100)),
    CompDetails),
ScoreCompetences is (CompDetails = [] ? 0 : sum_scores(CompDetails) / length(CompDetails)),

% Vérifier les intérêts
findall(ScoreInteret,
    (interet(Personne, Domaine, NiveauInteret),
     domaine_metier(Metier, Domaine),
     ScoreInteret is NiveauInteret * 20),
    InteretScores),
ScoreInteret is (InteretScores = [] ? 0 : sum_scores(InteretScores) / length(InteretScores)),

% Vérifier les traits de personnalité
findall(ScoreTrait,
    (trait_personnalite(Personne, Trait, NiveauTrait),
     trait_requis(Metier, Trait, NiveauRequisTrait),
     ScoreTrait is (NiveauTrait >= NiveauRequisTrait ? 100 : (NiveauTrait / NiveauRequisTrait) * 100)),
    TraitScores),
ScorePersonnalite is (TraitScores = [] ? 0 : sum_scores(TraitScores) / length(TraitScores)),

% Calcul du score global
Score is (ScoreEtude * 0.3 + ScoreCompetences * 0.4 + ScoreInteret * 0.2 + ScorePersonnalite * 0.1),
Details = details(
    etude(ScoreEtude, NiveauEtude, NiveauRequisEtude),
    competences(ScoreCompetences, CompDetails),
    interet(ScoreInteret),
    personnalite(ScorePersonnalite)
).

% 29. Recommander des métiers pour une personne
recommander_metiers(Personne, MetiersRecommandes) :-
findall(metier_score(Metier, Score, Details),
    (metier(Metier, _),
     compatible_avec_metier(Personne, Metier, Score, Details),
     Score >= 40),  % Seuil minimal de compatibilité
    MetiersRecommandes),
sort(2, @>=, MetiersRecommandes, MetiersTries).

% 30. Fonction utilitaire pour sommer les scores
sum_scores(Liste) :-
sum_scores(Liste, 0).
sum_scores([], Acc, Acc).
sum_scores([H|T], Acc, Result) :-
NewAcc is Acc + H,
sum_scores(T, NewAcc, Result).

% 31. Métiers et leurs détails (exemples)
metier(ingenieur_logiciel, informatique).
metier(data_scientist, informatique).
metier(medecin, sante).
metier(enseignant, education).

% 32. Compétences requises pour les métiers (exemples)
requiert_competence(programmation, ingenieur_logiciel, 4).
requiert_competence(analyse_donnees, data_scientist, 5).
requiert_competence(communication, enseignant, 3).

% 33. Traits de personnalité requis pour les métiers (exemples)
trait_requis(ingenieur_logiciel, logique, 4).
trait_requis(medecin, empathie, 5).
trait_requis(enseignant, patience, 4).

% 34. Écoles et formations (exemples)
ecole(epitech, informatique, paris).
ecole(sorbonne, sciences_humaines, paris).

formation(epitech, master, ingenierie_logiciel).
formation(sorbonne, licence, psychologie).

% 35. Détails des formations (exemples)
cout_formation(epitech, ingenierie_logiciel, 8000).
duree_formation(epitech, ingenierie_logiciel, 2).
cout_formation(sorbonne, psychologie, 5000).
duree_formation(sorbonne, psychologie, 3).

% 36. Point d'entrée principal
:- demarrer.