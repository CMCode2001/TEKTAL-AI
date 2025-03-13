% Importer le module lists
:- use_module(library(lists)).

% ======= MOTEUR D'INFÉRENCE AVEC PERSISTANCE DES DONNÉES UTILISATEUR =======

% 1. Déclarer les prédicats dynamiques pour stocker les informations de l'utilisateur
:- dynamic niveau_etude/2.
:- dynamic niveau_competence/3.
:- dynamic niveau_matiere/3.
:- dynamic interet/3.
:- dynamic trait_personnalite/3.

% 2. Point d'entrée pour interagir avec le moteur d'inférence
demarrer :-
    format('~nBienvenue dans le moteur d\'inférence interactif!~n', []),
    format('Je vais vous poser des questions pour vous aider à explorer les métiers, les compétences, les formations, etc.~n', []),
    format('Pour quitter, tapez "quitter" à tout moment. Vous pouvez également demander de l\'aide à tout moment.~n~n', []),
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

traiter_choix(_) :-
    format('Choix invalide. Veuillez réessayer. Assurez-vous de choisir un numéro valide dans le menu.~n', []).

% 5. Gérer le profil utilisateur (menu)
gerer_profil :-
    repeat,
    format('~n=== GÉRER VOTRE PROFIL ===~n', []),
    format('1. Créer un nouveau profil~n', []),
    format('2. Charger un profil existant~n', []),
    format('3. Modifier un profil existant~n', []),
    format('4. Supprimer un profil~n', []),
    format('5. Afficher les profils disponibles~n', []),
    format('0. Retour au menu principal~n', []),
    format('~nVotre choix: ', []),
    read(Choix),
    (Choix = 0 ->
        !
    ; Choix = 1 ->
        creer_profil
    ; Choix = 2 ->
        charger_profil_fichier
    ; Choix = 3 ->
        modifier_profil
    ; Choix = 4 ->
        supprimer_profil
    ; Choix = 5 ->
        lister_profils
    ;
        format('Choix invalide. Veuillez réessayer.~n', [])
    ),
    Choix \= 0.

% 6. Créer un nouveau profil
creer_profil :-
    format('~n=== CRÉER UN NOUVEAU PROFIL ===~n', []),
    format('Entrez votre nom: ', []),
    read(Nom),
    % Vérifier si le fichier profil existe déjà
    atomic_list_concat(['profils/', Nom, '.pl'], NomFichier),
    (exists_file(NomFichier) ->
        format("Un profil avec ce nom existe déjà. Voulez-vous l\'écraser? (oui/non): ", []),
        read(Reponse),
        (Reponse = oui ->
            true
        ;
            format('Création de profil annulée. Vous pouvez réessayer ou retourner au menu principal.~n', []),
            fail
        )
    ;
        true
    ),
    
    % Collecter les informations du profil
    format("Entrez votre niveau d\'études (1=bac, 2=bac+2, 3=licence, 4=master, 5=doctorat): ", []),
    read(NiveauEtude),
    
    % Nettoyer les données existantes pour ce profil
    retractall(niveau_etude(Nom, _)),
    retractall(niveau_competence(Nom, _, _)),
    retractall(niveau_matiere(Nom, _, _)),
    retractall(interet(Nom, _, _)),
    retractall(trait_personnalite(Nom, _, _)),
    
    % Ajouter le niveau d'étude
    assertz(niveau_etude(Nom, NiveauEtude)),
    
    % Collecter les compétences
    format('Entrez vos compétences (sous la forme competence-niveau, par exemple programmation-4):~n', []),
    format('Tapez "fin" pour terminer.~n', []),
    charger_competences(Nom),
    
    % Collecter les matières
    format('Entrez vos niveaux de matières (sous la forme matiere-niveau, par exemple mathematiques-4):~n', []),
    format('Tapez "fin" pour terminer.~n', []),
    charger_matieres(Nom),
    
    % Collecter les intérêts
    format('Entrez vos intérêts (sous la forme domaine-niveau, par exemple informatique-5):~n', []),
    format('Tapez "fin" pour terminer.~n', []),
    charger_interets(Nom),
    
    % Collecter les traits de personnalité
    format('Entrez vos traits de personnalité (sous la forme trait-niveau, par exemple creativite-4):~n', []),
    format('Tapez "fin" pour terminer.~n', []),
    charger_traits_personnalite(Nom),
    
    % Sauvegarder le profil dans un fichier
    sauvegarder_profil(Nom),
    format('Profil de ~w créé et sauvegardé avec succès. Vous pouvez maintenant gérer votre profil ou explorer les métiers.~n', [Nom]).

% 7. Charger les compétences de l'utilisateur
charger_competences(Nom) :-
    repeat,
    format('> ', []),
    read_line_to_string(user_input, CompetenceNiveau),
    (CompetenceNiveau = "fin" ->
        !
    ;
        split_string(CompetenceNiveau, "-", "", [CompStr, NiveauStr]),
        atom_string(Comp, CompStr),
        number_string(Niveau, NiveauStr),
        assertz(niveau_competence(Nom, Comp, Niveau)),
        fail
    ).

% 8. Charger les matières de l'utilisateur
charger_matieres(Nom) :-
    repeat,
    format('> ', []),
    read_line_to_string(user_input, MatiereNiveau),
    (MatiereNiveau = "fin" ->
        !
    ;
        split_string(MatiereNiveau, "-", "", [MatStr, NiveauStr]),
        atom_string(Mat, MatStr),
        number_string(Niveau, NiveauStr),
        assertz(niveau_matiere(Nom, Mat, Niveau)),
        fail
    ).

% 9. Charger les intérêts de l'utilisateur
charger_interets(Nom) :-
    repeat,
    format('> ', []),
    read_line_to_string(user_input, DomaineNiveau),
    (DomaineNiveau = "fin" ->
        !
    ;
        split_string(DomaineNiveau, "-", "", [DomStr, NiveauStr]),
        atom_string(Dom, DomStr),
        number_string(Niveau, NiveauStr),
        assertz(interet(Nom, Dom, Niveau)),
        fail
    ).

% 10. Charger les traits de personnalité de l'utilisateur
charger_traits_personnalite(Nom) :-
    repeat,
    format('> ', []),
    read_line_to_string(user_input, TraitNiveau),
    (TraitNiveau = "fin" ->
        !
    ;
        split_string(TraitNiveau, "-", "", [TraitStr, NiveauStr]),
        atom_string(Trait, TraitStr),
        number_string(Niveau, NiveauStr),
        assertz(trait_personnalite(Nom, Trait, Niveau)),
        fail
    ).

% 11. Sauvegarder le profil dans un fichier
sauvegarder_profil(Nom) :-
    % Assurer que le répertoire profils existe
    (exists_directory('profils') ->
        true
    ;
        make_directory('profils')
    ),
    
    % Construire le nom du fichier
    atomic_list_concat(['profils/', Nom, '.pl'], NomFichier),
    
    % Ouvrir le fichier en écriture
    open(NomFichier, write, Stream),
    
    % Écrire les faits du profil
    format(Stream, '% Profil de ~w~n', [Nom]),
    
    % Écrire le niveau d'étude
    (niveau_etude(Nom, Niveau) ->
        format(Stream, 'niveau_etude(~w, ~w).~n', [Nom, Niveau])
    ;
        true
    ),
    
    % Écrire les compétences
    findall(competence(C, N), niveau_competence(Nom, C, N), Competences),
    forall(member(competence(C, N), Competences),
        format(Stream, 'niveau_competence(~w, ~w, ~w).~n', [Nom, C, N])),
    
    % Écrire les matières
    findall(matiere(M, N), niveau_matiere(Nom, M, N), Matieres),
    forall(member(matiere(M, N), Matieres),
        format(Stream, 'niveau_matiere(~w, ~w, ~w).~n', [Nom, M, N])),
    
    % Écrire les intérêts
    findall(int(D, N), interet(Nom, D, N), Interets),
    forall(member(int(D, N), Interets),
        format(Stream, 'interet(~w, ~w, ~w).~n', [Nom, D, N])),
    
    % Écrire les traits de personnalité
    findall(trait(T, N), trait_personnalite(Nom, T, N), Traits),
    forall(member(trait(T, N), Traits),
        format(Stream, 'trait_personnalite(~w, ~w, ~w).~n', [Nom, T, N])),
    
    % Fermer le fichier
    close(Stream).

% 12. Charger un profil depuis un fichier
charger_profil_fichier :-
    format('~n=== CHARGER UN PROFIL EXISTANT ===~n', []),
    % Vérifier que le répertoire profils existe
    (exists_directory('profils') ->
        true
    ;
        format('Aucun profil n\'a encore été créé.~n', []),
        fail
    ),
    
    % Lister les profils disponibles
    lister_profils,
    
    % Demander le nom du profil à charger
    format('Entrez le nom du profil à charger: ', []),
    read(Nom),
    
    % Vérifier si le fichier existe
    atomic_list_concat(['profils/', Nom, '.pl'], NomFichier),
    (exists_file(NomFichier) ->
        % Nettoyer les données existantes pour ce profil
        retractall(niveau_etude(Nom, _)),
        retractall(niveau_competence(Nom, _, _)),
        retractall(niveau_matiere(Nom, _, _)),
        retractall(interet(Nom, _, _)),
        retractall(trait_personnalite(Nom, _, _)),
        
        % Charger le fichier
        consult(NomFichier),
        format('Profil de ~w chargé avec succès.~n', [Nom])
    ;
        format('Le profil ~w n\'existe pas.~n', [Nom]),
        fail
    ).

% 13. Modifier un profil existant
modifier_profil :-
    format('~n=== MODIFIER UN PROFIL EXISTANT ===~n', []),
    % Vérifier que le répertoire profils existe
    (exists_directory('profils') ->
        true
    ;
        format('Aucun profil n\'a encore été créé.~n', []),
        fail
    ),
    
    % Lister les profils disponibles
    lister_profils,
    
    % Demander le nom du profil à modifier
    format('Entrez le nom du profil à modifier: ', []),
    read(Nom),
    
    % Vérifier si le fichier existe
    atomic_list_concat(['profils/', Nom, '.pl'], NomFichier),
    (exists_file(NomFichier) ->
        % Charger le fichier
        consult(NomFichier),
        
        % Menu de modification
        repeat,
        format('~n=== MODIFICATION DU PROFIL DE ~w ===~n', [Nom]),
        format('1. Modifier le niveau d\'études~n', []),
        format('2. Modifier/Ajouter des compétences~n', []),
        format('3. Modifier/Ajouter des matières~n', []),
        format('4. Modifier/Ajouter des intérêts~n', []),
        format('5. Modifier/Ajouter des traits de personnalité~n', []),
        format('0. Terminer et sauvegarder~n', []),
        format('~nVotre choix: ', []),
        read(ChoixMod),
        
        (ChoixMod = 0 ->
            sauvegarder_profil(Nom),
            format('Modifications sauvegardées avec succès.~n', []),
            !
        ; ChoixMod = 1 ->
            format('Niveau d\'études actuel: ', []),
            (niveau_etude(Nom, NiveauActuel) ->
                format('~w~n', [NiveauActuel])
            ;
                format('Non défini~n', [])
            ),
            format('Entrez le nouveau niveau d\'études (1-5): ', []),
            read(NouveauNiveau),
            retractall(niveau_etude(Nom, _)),
            assertz(niveau_etude(Nom, NouveauNiveau))
        ; ChoixMod = 2 ->
            format('Compétences actuelles:~n', []),
            (findall(C-N, niveau_competence(Nom, C, N), Competences) ->
                forall(member(C-N, Competences),
                    format('  - ~w: ~w~n', [C, N]))
            ;
                format('  Aucune compétence définie~n', [])
            ),
            format('Entrez les nouvelles compétences (format: competence-niveau):~n', []),
            format('Tapez "fin" pour terminer.~n', []),
            charger_competences(Nom)
        ; ChoixMod = 3 ->
            format('Matières actuelles:~n', []),
            (findall(M-N, niveau_matiere(Nom, M, N), Matieres) ->
                forall(member(M-N, Matieres),
                    format('  - ~w: ~w~n', [M, N]))
            ;
                format('  Aucune matière définie~n', [])
            ),
            format('Entrez les nouvelles matières (format: matiere-niveau):~n', []),
            format('Tapez "fin" pour terminer.~n', []),
            charger_matieres(Nom)
        ; ChoixMod = 4 ->
            format('Intérêts actuels:~n', []),
            (findall(D-N, interet(Nom, D, N), Interets) ->
                forall(member(D-N, Interets),
                    format('  - ~w: ~w~n', [D, N]))
            ;
                format('  Aucun intérêt défini~n', [])
            ),
            format('Entrez les nouveaux intérêts (format: domaine-niveau):~n', []),
            format('Tapez "fin" pour terminer.~n', []),
            charger_interets(Nom)
        ; ChoixMod = 5 ->
            format('Traits de personnalité actuels:~n', []),
            (findall(T-N, trait_personnalite(Nom, T, N), Traits) ->
                forall(member(T-N, Traits),
                    format('  - ~w: ~w~n', [T, N]))
            ;
                format('  Aucun trait de personnalité défini~n', [])
            ),
            format('Entrez les nouveaux traits (format: trait-niveau):~n', []),
            format('Tapez "fin" pour terminer.~n', []),
            charger_traits_personnalite(Nom)
        ;
            format('Choix invalide. Veuillez réessayer.~n', [])
        ),
        ChoixMod \= 0
    ;
        format('Le profil ~w n\'existe pas.~n', [Nom]),
        fail
    ).

% 14. Supprimer un profil
supprimer_profil :-
    format('~n=== SUPPRIMER UN PROFIL ===~n', []),
    % Vérifier que le répertoire profils existe
    (exists_directory('profils') ->
        true
    ;
        format('Aucun profil n\'a encore été créé.~n', []),
        fail
    ),
    
    % Lister les profils disponibles
    lister_profils,
    
    % Demander le nom du profil à supprimer
    format('Entrez le nom du profil à supprimer: ', []),
    read(Nom),
    
    % Vérifier si le fichier existe
    atomic_list_concat(['profils/', Nom, '.pl'], NomFichier),
    (exists_file(NomFichier) ->
        format('Êtes-vous sûr de vouloir supprimer le profil de ~w? (oui/non): ', [Nom]),
        read(Confirmation),
        (Confirmation = oui ->
            delete_file(NomFichier),
            % Nettoyer également les faits en mémoire
            retractall(niveau_etude(Nom, _)),
            retractall(niveau_competence(Nom, _, _)),
            retractall(niveau_matiere(Nom, _, _)),
            retractall(interet(Nom, _, _)),
            retractall(trait_personnalite(Nom, _, _)),
            format('Profil de ~w supprimé avec succès.~n', [Nom])
        ;
            format('Suppression annulée.~n', [])
        )
    ;
        format('Le profil ~w n\'existe pas.~n', [Nom]),
        fail
    ).

% 15. Lister les profils disponibles
lister_profils :-
    format('~n=== PROFILS DISPONIBLES ===~n', []),
    % Vérifier que le répertoire profils existe
    (exists_directory('profils') ->
        findall(Nom, (
            directory_files('profils', Files),
            member(File, Files),
            atom_concat(Nom, '.pl', File),
            Nom \= '.',
            Nom \= '..'
        ), Noms),
        
        (Noms = [] ->
            format('Aucun profil n\'est disponible.~n', [])
        ;
            forall(member(Nom, Noms),
                format('  - ~w~n', [Nom]))
        )
    ;
        format('Aucun profil n\'a encore été créé.~n', [])
    ).

% 16. Explorer les métiers
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
    ),
    Choix \= 0.

% 17. Évaluer la compatibilité avec un métier (interactif)
evaluer_compatibilite_interactive :-
    format('~n=== ÉVALUER LA COMPATIBILITÉ AVEC UN MÉTIER ===~n', []),
    % Lister les profils disponibles
    lister_profils,
    format('Entrez votre nom: ', []),
    read(Personne),
    % Vérifier si le profil est chargé, sinon le charger
    (niveau_etude(Personne, _) ->
        true
    ;
        format('Le profil n\'est pas chargé. Tentative de chargement...~n', []),
        atomic_list_concat(['profils/', Personne, '.pl'], NomFichier),
        (exists_file(NomFichier) ->
            consult(NomFichier),
            format('Profil chargé.~n', [])
        ;
            format('Le profil ~w n\'existe pas. Veuillez le créer d\'abord.~n', [Personne]),
            fail
        )
    ),
    format('Entrez le métier que vous souhaitez évaluer: ', []),
    read(Metier),
    % Vérifier si le métier existe
    (metier(Metier, _) ->
        evaluer_compatibilite(Personne, Metier)
    ;
        format('Le métier ~w n\'existe pas dans la base de connaissances.~n', [Metier]),
        fail
    ).

% 18. Recommander des métiers (interactif) - Corrigé
recommander_metiers_interactive :-
    format('~n=== RECOMMANDER DES MÉTIERS ===~n', []),
    % Lister les profils disponibles
    lister_profils,
    format('Entrez votre nom: ', []),
    read(Personne),
    % Vérifier si le profil est chargé, sinon le charger
    (niveau_etude(Personne, _) ->
        true
    ;
        format('Le profil n\'est pas chargé. Tentative de chargement...~n', []),
        atomic_list_concat(['profils/', Personne, '.pl'], NomFichier),
        (exists_file(NomFichier) ->
            consult(NomFichier),
            format('Profil chargé.~n', [])
        ;
            format('Le profil ~w n\'existe pas. Veuillez le créer d\'abord.~n', [Personne]),
            fail
        )
    ),
    % Obtenir les recommandations et les afficher
    findall(metier_score(Metier, Score, Details),
        (metier(Metier, _),
         compatible_avec_metier(Personne, Metier, Score, Details),
         Score >= 40),  % Seuil minimal de compatibilité
        MetiersRecommandes),
    sort(2, @>=, MetiersRecommandes, MetiersTries),
    
    format('~n=== Métiers recommandés pour ~w ===~n', [Personne]),
    (MetiersTries = [] ->
        format('Aucun métier compatible trouvé.~n', [])
    ;
        forall(member(metier_score(Metier, Score, _), MetiersTries),
            format('  - ~w (Score de compatibilité: ~2f)~n', [Metier, Score])),
        
        format('~nVoulez-vous voir les détails pour un métier spécifique? (nom du métier/non): ', []),
        read(Reponse),
        (Reponse \= non ->
            member(metier_score(Reponse, Score, Details), MetiersTries),
            format('~nDétails de compatibilité pour ~w (Score global: ~2f):~n', [Reponse, Score]),
            afficher_details_compatibilite(Details)
        ;
            true
        )
    ).

% 28. Compatibilité avec un métier - Corrigé
compatible_avec_metier(Personne, Metier, Score, Details) :-
    % Vérifier le niveau d'études
    (niveau_etude(Personne, NiveauEtude) -> true ; NiveauEtude = 0),
    (niveau_etude_requis(Metier, NiveauRequisEtude) -> true ; NiveauRequisEtude = 1),
    ScoreEtude is (NiveauEtude >= NiveauRequisEtude ? 100 : (NiveauEtude / NiveauRequisEtude) * 100),

    % Vérifier les compétences
    findall(detail(Comp, NiveauRequis, NiveauPersonne, ScoreComp),
        (requiert_competence(Comp, Metier, NiveauRequis),
         (niveau_competence(Personne, Comp, NiveauPersonne) -> true ; NiveauPersonne = 0),
         ScoreComp is (NiveauPersonne >= NiveauRequis ? 100 : (NiveauPersonne / NiveauRequisEtude) * 100)),
        CompDetails),
    
    % Calculer le score des compétences
    (CompDetails = [] -> 
        ScoreCompetences = 0
    ; 
        findall(SC, member(detail(_, _, _, SC), CompDetails), ScoresList),
        sum_list(ScoresList, SumScores),
        length(CompDetails, Len),
        ScoreCompetences is SumScores / Len
    ),

    % Vérifier les intérêts
    (metier(Metier, Domaine) -> true ; Domaine = inconnu),
    findall(SI,
        (interet(Personne, D, NiveauInteret),
         (D = Domaine -> SI is NiveauInteret * 20 ; SI = 0)),
        InteretScores),
    
    % Calculer le score des intérêts
    (InteretScores = [] -> 
        ScoreInteret = 0
    ; 
        sum_list(InteretScores, SumInteret),
        length(InteretScores, LenInteret),
        ScoreInteret is SumInteret / LenInteret
    ),

    % Vérifier les traits de personnalité
    findall(ST,
        (trait_requis(Metier, Trait, NiveauRequisTrait),
         (trait_personnalite(Personne, Trait, NiveauTrait) -> true ; NiveauTrait = 0),
         ST is (NiveauTrait >= NiveauRequisTrait ? 100 : (NiveauTrait / NiveauRequisTrait) * 100)),
        TraitScores),
    
    % Calculer le score de personnalité
    (TraitScores = [] -> 
        ScorePersonnalite = 0
    ; 
        sum_list(TraitScores, SumTrait),
        length(TraitScores, LenTrait),
        ScorePersonnalite is SumTrait / LenTrait
    ),

    % Calcul du score global
    Score is (ScoreEtude * 0.3 + ScoreCompetences * 0.4 + ScoreInteret * 0.2 + ScorePersonnalite * 0.1),
    Details = details(
        etude(ScoreEtude, NiveauEtude, NiveauRequisEtude),
        competences(ScoreCompetences, CompDetails),
        interet(ScoreInteret),
        personnalite(ScorePersonnalite)
    ).

% 30. Fonction utilitaire pour sommer les scores (remplacé par prédicat intégré)
% Supprimer la redéfinition de sum_list/2, car elle est déjà fournie par le module lists.

% Ajouté: niveau d'étude requis pour les métiers (exemples)
niveau_etude_requis(ingenieur_logiciel, 4).
niveau_etude_requis(data_scientist, 5).
niveau_etude_requis(medecin, 5).
niveau_etude_requis(enseignant, 3).

% Ajouté: domaine des métiers pour les intérêts
domaine_metier(ingenieur_logiciel, informatique).
domaine_metier(data_scientist, informatique).
domaine_metier(medecin, sante).
domaine_metier(enseignant, education).

% 19. Explorer les formations
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
    ),
    Choix \= 0.

% 20. Lister tous les métiers disponibles
lister_metiers :-
    format('~n=== Liste des métiers disponibles ===~n', []),
    forall(metier(Metier, Domaine),
        format('  - ~w (Domaine: ~w)~n', [Metier, Domaine])).

% 21. Lister les métiers dans un domaine spécifique
lister_metiers_domaine(Domaine) :-
    format('~n=== Métiers dans le domaine ~w ===~n', [Domaine]),
    (metier(_, Domaine) ->
        forall(metier(Metier, Domaine),
            format('  - ~w~n', [Metier]))
    ;
        format('Aucun métier trouvé dans le domaine ~w.~n', [Domaine])
    ).

% 22. Afficher les détails d'un métier spécifique
details_metier(Metier) :-
    format('~n=== Détails du métier ~w ===~n', [Metier]),
    (metier(Metier, Domaine) ->
        format('Domaine: ~w~n', [Domaine]),
        (description(Metier, Description) ->
            format('Description: ~w~n', [Description])
        ;
            format('Description: Non disponible~n', [])
        ),
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
            format('  - ~w (Niveau requis: ~w)~n', [Comp, NiveauRequis]))
    ;
        format('Le métier ~w n\'existe pas dans la base de connaissances.~n', [Metier])
    ).

% 23. Évaluer la compatibilité d'une personne avec un métier
evaluer_compatibilite(Personne, Metier) :-
    format('~n=== Évaluation de la compatibilité de ~w avec ~w ===~n', [Personne, Metier]),
    (compatible_avec_metier(Personne, Metier, Score, Details) ->
        format('Score de compatibilité: ~2f/100~n', [Score]),
        afficher_details_compatibilite(Details)
    ;
        format('Impossible d\'évaluer la compatibilité pour ~w avec ~w.~n', [Personne, Metier])
    ).

% 24. Afficher les détails de compatibilité
afficher_details_compatibilite(Details) :-
    Details = details(
        etude(ScoreEtude, NiveauEtude, NiveauRequisEtude),
        competences(ScoreCompetences, CompDetails),
        interet(ScoreInteret),
        personnalite(ScorePersonnalite)
    ),
    format('Détails:~n', []),
    format('  - Niveau d\'études: Score = ~2f/100 (Actuel: ~w, Requis: ~w)~n', 
           [ScoreEtude, NiveauEtude, NiveauRequisEtude]),
    format('  - Compétences: Score global = ~2f/100~n', [ScoreCompetences]),
    forall(member(detail(Comp, NiveauRequis, NiveauPersonne, ScoreComp), CompDetails),
        format('    * ~w: Niveau actuel = ~w, Niveau requis = ~w, Score = ~2f/100~n', 
               [Comp, NiveauPersonne, NiveauRequis, ScoreComp])),
    format('  - Intérêt: Score = ~2f/100~n', [ScoreInteret]),
    format('  - Personnalité: Score = ~2f/100~n', [ScorePersonnalite]).

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