
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(odbc)).
:- use_module(library(http/http_client)).
:- use_module(library(http/json)).


:- consult('base_de_connaissance.pl').
:- consult('moteur_inference.pl').

:- use_module(base_de_connaissance, [matiere_requise/4]).

% Démarrer le serveur sur le port 8000
server(8000) :- http_server(http_dispatch, [port(8000)]).

% --- Connexion à la base de données ---
connect_db :-
    odbc_connect('expert_db', _, [user('root'), password(''), alias(expert_db), open(once)]).




% --- Définition des endpoints REST ---
:- http_handler(root(ajouter_utilisateur), ajouter_utilisateur, [method(post)]).
:- http_handler(root(traiter_notes_utilisateur), traiter_notes_utilisateur, [method(post)]).

:- http_handler(root(orientation_professionnelle), orientation_professionnelle, []).

:- http_handler(root(competences), get_competences, []).
:- http_handler(root(domaines), get_domaines, []).
:- http_handler(root(personnalites), get_personnalites, []).
:- http_handler(root(filieres), get_filieres, []).
:- http_handler(root(niveaux_etude), get_niveaux_etude, []).


% Convertir une liste d'atomes en une liste de chaînes
atoms_to_strings([], []).
atoms_to_strings([H | T], [HStr | TStr]) :-
    atom_string(H, HStr),
    atoms_to_strings(T, TStr).

% Convertir la liste en JSON
convert_matieres_to_json(Matieres, MatieresJSON) :-
    atoms_to_strings(Matieres, MatieresStrings),  % Convertir les atomes en chaînes
    atom_json_dict(MatieresJSON, _{matiere_requise: MatieresStrings}, []).

% Exemple d'utilisation
update_user_matieres(ID, Matieres) :-
    convert_matieres_to_json(Matieres, MatieresStr),

    % Préparer et exécuter la requête SQL
    odbc_prepare(expert_db, 
        'UPDATE utilisateurs SET resultat = ? WHERE id = ?', 
        [varchar, integer], 
        Statement),
    odbc_execute(Statement, [MatieresStr, ID]),
    odbc_free_statement(Statement).


% --- ORIENTATION ACADEMIQUE ---
ajouter_utilisateur(Request) :-
    http_read_json_dict(Request, Dict),
    _{
        nom: Nom, niveau_etude: Niveau, filiere: Filiere, domaine_interet: Domaine
    } :< Dict,

    % Insérer l'utilisateur en base
   connect_db,
    odbc_prepare(expert_db, 
                 'INSERT INTO utilisateurs (nom, niveau_etude, filiere, domaine_interet, type) VALUES (?, ?, ?, ?, ?)', 
                 [varchar, varchar, varchar, varchar, varchar], 
                 Statement),
    odbc_execute(Statement, [Nom, Niveau, Filiere, Domaine, 'academique']),
    odbc_free_statement(Statement),

    % Récupérer l'ID généré
    odbc_query(expert_db, "SELECT LAST_INSERT_ID()", row(ID)),

    % Déterminer les matières requises
    atom_string(DomaineAtom, Domaine),
    atom_string(NiveauAtom, Niveau),
    atom_string(FiliereAtom, Filiere),
    matiere_requise(DomaineAtom, NiveauAtom, FiliereAtom, Matieres),

    update_user_matieres(ID, Matieres),

    % Répondre avec les matières requises
    reply_json_dict(_{
        message: "Veuillez fournir vos notes pour ces matières.",
        id_utilisateur: ID,
        resultat: Matieres
    }).


recuperer_matieres_utilisateur(ID, Matieres) :-
    % Préparer la requête SQL
    connect_db,
    odbc_prepare(expert_db, 
                 'SELECT resultat FROM utilisateurs WHERE id = ?', 
                 [integer], 
                 Statement),

    % Exécuter la requête avec l'ID donné et récupérer directement le résultat
    (   odbc_execute(Statement, [ID], row(MatieresJSON))
    ->  % Convertir le JSON en dictionnaire Prolog
        atom_json_dict(MatieresJSON, MatieresDict, []),
        Matieres = MatieresDict.matiere_requise
    ;   writeln("Erreur : utilisateur introuvable"), fail
    ),

    % Libérer la requête préparée
    odbc_free_statement(Statement).

% Vérifie si toutes les matières requises ont une note suffisante
% Cas de base : si plus de matières à vérifier, c'est valide
valide_notes([], _).

% Cas récursif : vérifier la note pour chaque matière
valide_notes([Matiere | Rest], Notes) :-
    % Convertir "Matiere" (qui est une chaîne de caractères) en atome
    atom_string(MatiereAtom, Matiere),  % "math" devient atom(math)
    
    % Récupérer la note de la matière dans le dictionnaire
    get_dict(MatiereAtom, Notes, Note),

    % Vérifier si la note est suffisante (exemple : seuil = 10/20)
    Note >= 10,

    % Vérifier les autres matières
    valide_notes(Rest, Notes).

% Connexion à la base de données et récupération du domaine et du niveau par ID
recuperer_domaine_niveau(ID, Domaine, Niveau) :-
    % Préparer la requête SQL pour récupérer le domaine et le niveau
    connect_db,
    odbc_prepare(expert_db, 
                 'SELECT domaine_interet, niveau_etude FROM utilisateurs WHERE id = ?', 
                 [integer], 
                 Statement),
    
    % Exécuter la requête avec l'ID de l'utilisateur
    odbc_execute(Statement, [ID],row(Domaine, Niveau)),
    odbc_free_statement(Statement).


traiter_notes_utilisateur(Request) :-
    http_read_json_dict(Request, Dict),
    _{ id_utilisateur: ID, notes: Notes} :< Dict,  % Récupère les notes envoyées par l'utilisateur

    % Récupérer les matières requises
    recuperer_matieres_utilisateur(ID, Matieres), 

    recuperer_domaine_niveau(ID, Domaine, Niveau),

    % Vérifier si les notes sont valides
    (valide_notes(Matieres, Notes) ->
        (
            etablissement(Domaine, Etablissements),
            parcours(Domaine, Niveau, Parcours),
            atom_concat("Vous pouvez poursuivre en ", Domaine, Message),
            ResultatFinal = _{
                message: Message,
                etablissements_recommandes: Etablissements,
                parcours_possibles: Parcours,
                matieres_requises: Matieres
            },
            atom_json_dict(ResultatStr, ResultatFinal, []),

            % Mettre à jour le résultat en JSON
            connect_db,
            odbc_prepare(expert_db, 
                         'UPDATE utilisateurs SET resultat = ? WHERE id = ?', 
                         [varchar, integer], 
                         Statement),
            odbc_execute(Statement, [ResultatStr, ID]),
            odbc_free_statement(Statement),

            reply_json_dict(ResultatFinal)
        )
    ;
        % Si les notes ne sont pas suffisantes
        ResultatEchec = _{
            message: "Désolé, vous ne remplissez pas les conditions pour " + Domaine
        },
        atom_json_dict(ResultatJSONEchec, ResultatEchec, []),

        % Mettre à jour le champ `resultat`
        format(atom(RequeteEchec), "UPDATE utilisateurs SET resultat='~w' WHERE id=~w", [ResultatJSONEchec, ID]),
        odbc_query(expert_db, RequeteEchec, affected(_)),

        reply_json_dict(ResultatEchec)
    ).



% --- ORIENTATION PROFESSIONNELLE ---
orientation_professionnelle(Request) :-
    http_read_json_dict(Request, Dict),
    _{nom: NomStr, niveau_etude: NiveauStr, filiere: FiliereStr, competences: CompetencesStr, traits_personnalite: TraitsStr} :< Dict,

    % Convertir les chaînes en atoms
    atom_string(Niveau, NiveauStr),
    atom_string(Filiere, FiliereStr),

    % Si les compétences et les traits sont des listes de strings, les convertir en atoms
    maplist(atom_string, Competences, CompetencesStr),
    maplist(atom_string, Traits, TraitsStr),

    % Appel de la fonction recommandation et récupération de la liste
    recommander_domaines_pertinents(Niveau, Filiere, Competences, Traits, DomainesPertinents),

    % Vérifier s'il y a des résultats et renvoyer la réponse JSON
    (DomainesPertinents \= [] ->
        Resultat = _{domaines_suggeres: DomainesPertinents},
        atom_json_dict(ResultatStr, Resultat, []),
        save_utilisateur(NomStr, NiveauStr, FiliereStr, 'professionnelle', ResultatStr,  Competences, Traits),
        reply_json_dict(Resultat)
    ;
        reply_json_dict(_{message: "Aucun domaine correspondant trouvé"})).


% --- AJOUT D'UN UTILISATEUR ---
save_utilisateur(Nom, NiveauEtude, Filiere, Type, Resultat, Competences, Traits) :-
    connect_db,

    % Insertion de l'utilisateur dans la table 'utilisateurs'
    odbc_prepare(expert_db, 
        'INSERT INTO utilisateurs (nom, niveau_etude, filiere, type, resultat) VALUES (?, ?, ?, ?, ?)',
        [varchar, varchar, varchar, varchar, varchar], 
        Statement1),
    odbc_execute(Statement1, [Nom, NiveauEtude, Filiere, Type, Resultat]),
    odbc_free_statement(Statement1),

    % Récupérer l'ID de l'utilisateur nouvellement inséré
    odbc_query(expert_db, 'SELECT LAST_INSERT_ID()', row(IdUtilisateur)),
    
    % Insérer chaque compétence dans la table 'competences_utilisateur'
    maplist(insert_competence(IdUtilisateur), Competences),
    
    % Insérer chaque trait dans la table 'personnalite_utilisateur'
    maplist(insert_trait(IdUtilisateur), Traits).


% Insérer une compétence dans la table 'competences_utilisateur'
insert_competence(IdUtilisateur, Competence) :-
    odbc_prepare(expert_db, 
        'INSERT INTO competences_utilisateur (utilisateur_id, competence) VALUES (?, ?)',
        [integer, varchar], 
        Statement),
    (   odbc_execute(Statement, [IdUtilisateur, Competence])
    ->  odbc_free_statement(Statement)
    ;   write('Erreur lors de l\'insertion de la compétence: ', Competence), nl
    ).

% Insérer un trait dans la table 'personnalite_utilisateur'
insert_trait(IdUtilisateur, Trait) :-
    odbc_prepare(expert_db, 
        'INSERT INTO personnalite_utilisateur (utilisateur_id, trait) VALUES (?, ?)',
        [integer, varchar], 
        Statement),
    (   odbc_execute(Statement, [IdUtilisateur, Trait])
    ->  odbc_free_statement(Statement)
    ;   write('Erreur lors de l\'insertion du trait: ', Trait), nl
    ).

% strings_to_atoms([], []).

% % Cas récursif : convertir chaque élément de la liste
% strings_to_atoms([Str|Strs], [Atom|Atoms]) :-
%     atom_string(Atom, Str),          % Convertir la chaîne en atome
%     strings_to_atoms(Strs, Atoms). 



get_competences(Request) :-
    findall(Competence, competence_technique_domaine(Competence, _), Competences),
    reply_json_dict(_{competences: Competences}).


get_domaines(Request) :-
    findall(Domaine, correspondance_filiere_domaine(_, _, Domaine), DomainesBruts),
    list_to_set(DomainesBruts, Domaines), % Évite les doublons
    reply_json_dict(_{domaines: Domaines}).


get_personnalites(Request) :-
    findall(Personnalite, personnalite_domaine(Personnalite, _), Personnalites),
    reply_json_dict(_{personnalites: Personnalites}).


get_filieres(Request) :-
    findall(Filiere, correspondance_filiere_domaine(_, Filiere, _), FilieresBruts),
    list_to_set(FilieresBruts, Filieres),
    reply_json_dict(_{filieres: Filieres}).


get_niveaux_etude(Request) :-
    findall(Niveau, correspondance_filiere_domaine(Niveau, _, _), NiveauxBruts),
    list_to_set(NiveauxBruts, Niveaux),
    reply_json_dict(_{niveaux_etude: Niveaux}).



% Lancer le serveur automatiquement au démarrage
:- initialization(server(8000)).