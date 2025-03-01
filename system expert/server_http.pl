:- set_prolog_flag(encoding, utf8).

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(odbc)).

:- use_module(moteur_inference).  % Importer le moteur d'inférence
:- use_module(knowledge_base). 
:- dynamic profile_exists/1.


% Démarrer le serveur sur le port 8000
server(8000) :- http_server(http_dispatch, [port(8000)]).
% Connexion à la base de données MySQL
connect_db :-
    odbc_connect('expert_db', _, [user('root'), password(''), alias(expert_db), open(once)]).

% Route pour le diagnostic
:- http_handler(root(lister_metiers), handle_metier, []).
:- http_handler(root(lister_domaines), handle_domaine, []).
:- http_handler(root(creer_profil), creer_profil_handler, [method(post)]).

handle_domaine(Request) :-
    findall(D, domaine(D), Doms),  % Récupérer toutes les maladies
    reply_json_dict(_{domaines: Doms}).    % Renvoyer le diagnostic en JSON

handle_metier(_Request) :-
    findall(_{metier: Metier, domaine: Domaine}, metier(Metier, Domaine), Metiers),
    reply_json_dict(_{metiers: Metiers}).


% handle_metier(Request) :-
%     findall(M, metier(M,_), Metiers),  % Récupérer toutes les maladies
%     reply_json_dict(_{metiers: Metiers}).    % Renvoyer le diagnostic en JSON


% Gestionnaire de la requête POST pour créer un profil
creer_profil_handler(Request) :-
    http_read_json_dict(Request, Dict),
    (   profile_exists(Dict.nom) ->
        reply_json_dict(_{message: "Un profil avec ce nom existe déjà."}, [status(400)])
    ;   creer_profil(Dict),
        save_profil(Dict),
        reply_json_dict(_{message: "Profil créé avec succès."}, [status(201)])
    ).




% Vérifie si un profil existe déjà
%profile_exists(Nom) :- profile_exists(Nom).
profile_exists(Nom) :- clause(profile_exists(Nom), true).


% Création du profil
creer_profil(Dict) :-
    assertz(niveau_etude(Dict.nom, Dict.niveau_etude)),
    assertz(profile_exists(Dict.nom)),
    ajouter_competences(Dict.nom, Dict.competences),
    ajouter_matieres(Dict.nom, Dict.matieres),
    ajouter_interets(Dict.nom, Dict.interets),
    ajouter_traits_personnalite(Dict.nom, Dict.traits).



%Ajouter une liste de competences pour un utilisateur
ajouter_competences(_, []).  % Cas de base : liste vide, rien à faire
ajouter_competences(Nom, [CompetenceNiveau | Rest]) :-
    split_string(CompetenceNiveau, "-", "", [CompStr, NiveauStr]),  % Sépare "programmation-4" en ["programmation", "4"]
    atom_string(Competence, CompStr),  % Convertit en atom (ex: programmation)
    number_string(Niveau, NiveauStr),  % Convertit en nombre (ex: 4)
    assertz(niveau_competence(Nom, Competence, Niveau)),  % Ajoute à la base de faits
    ajouter_competences(Nom, Rest).  % Récursion pour traiter le reste de la liste

% Ajouter une liste de matières pour un utilisateur
ajouter_matieres(_, []).  % Cas de base : liste vide, rien à faire
ajouter_matieres(Nom, [MatiereNiveau | Rest]) :-
    split_string(MatiereNiveau, "-", "", [MatStr, NiveauStr]),
    atom_string(Matiere, MatStr),
    number_string(Niveau, NiveauStr),
    assertz(niveau_matiere(Nom, Matiere, Niveau)),
    ajouter_matieres(Nom, Rest).  % Récursion

% Ajouter une liste d'intérêts pour un utilisateur
ajouter_interets(_, []).  % Cas de base
ajouter_interets(Nom, [DomaineNiveau | Rest]) :-
    split_string(DomaineNiveau, "-", "", [DomStr, NiveauStr]),
    atom_string(Domaine, DomStr),
    number_string(Niveau, NiveauStr),
    assertz(interet(Nom, Domaine, Niveau)),
    ajouter_interets(Nom, Rest).  % Récursion

% Ajouter une liste de traits de personnalité pour un utilisateur
ajouter_traits_personnalite(_, []).  % Cas de base
ajouter_traits_personnalite(Nom, [TraitNiveau | Rest]) :-
    split_string(TraitNiveau, "-", "", [TraitStr, NiveauStr]),
    atom_string(Trait, TraitStr),
    number_string(Niveau, NiveauStr),
    assertz(trait_personnalite(Nom, Trait, Niveau)),
    ajouter_traits_personnalite(Nom, Rest).  % Récursion

% sauvegarder_donnees(Nom) :-
%     atomic_list_concat([Nom, '.pl'], Fichier),  % Construit le nom du fichier
%     open(Fichier, write, Stream),
    
%     % Sauvegarde des profils existants
%     format(Stream, 'profile_exists(~q).~n', [Nom]),
    
%     % Sauvegarde des niveaux d'études
%     forall(niveau_etude(Nom, Niveau), 
%            format(Stream, 'niveau_etude(~q, ~q).~n', [Nom, Niveau])),
    
%     % Sauvegarde des compétences
%     forall(niveau_competence(Nom, Comp, Niveau), 
%            format(Stream, 'niveau_competence(~q, ~q, ~q).~n', [Nom, Comp, Niveau])),
    
%     % Sauvegarde des matières
%     forall(niveau_matiere(Nom, Mat, Niveau), 
%            format(Stream, 'niveau_matiere(~q, ~q, ~q).~n', [Nom, Mat, Niveau])),
    
%     % Sauvegarde des intérêts
%     forall(interet(Nom, Dom, Niveau), 
%            format(Stream, 'interet(~q, ~q, ~q).~n', [Nom, Dom, Niveau])),
    
%     % Sauvegarde des traits de personnalité
%     forall(trait_personnalite(Nom, Trait, Niveau), 
%            format(Stream, 'trait_personnalite(~q, ~q, ~q).~n', [Nom, Trait, Niveau])),
    
%     close(Stream),
%     format('Données de ~w sauvegardées dans ~w.~n', [Nom, Fichier]).

% Insérer un profil dans la base de données
save_profil(Nom, NiveauEtude) :-
    connect_db,
    odbc_prepare(expert_db, 
                'INSERT INTO profils (nom, niveau_etude) VALUES (?, ?)', 
                [varchar, integer], 
                Statement),
    odbc_execute(Statement, [Nom, NiveauEtude]),
    odbc_free_statement(Statement).


save_competences(_, []) :- !.  % Fin de la liste
save_competences(Profil, [CompetenceNiveau | Reste]) :-
    split_string(CompetenceNiveau, "-", "", [CompStr, NiveauStr]), % Séparer "competence-niveau"
    atom_string(Competence, CompStr),  % Convertir la compétence en atom
    number_string(Niveau, NiveauStr),  % Convertir le niveau en nombre
    connect_db,
    odbc_prepare(expert_db, 
                 'INSERT INTO competences (profil_id, competence, niveau) VALUES (?, ?, ?)', 
                 [integer, varchar, integer], 
                 Statement),
    odbc_execute(Statement, [Profil, Competence, Niveau]),
    odbc_free_statement(Statement),
    save_competences(Profil, Reste).  % Récursivité pour le reste de la liste



% Même principe pour matières, intérêts et traits de personnalité
save_matieres(_, []) :- !.  % Fin de la liste
save_matieres(Profil, [MatiereNiveau | Reste]) :-
    split_string(MatiereNiveau, "-", "", [MatStr, NiveauStr]), % Séparer "matiere-niveau"
    atom_string(Matiere, MatStr),  % Convertir en atom
    number_string(Niveau, NiveauStr),  % Convertir en nombre
    connect_db,
    odbc_prepare(expert_db, 
                 'INSERT INTO matieres (profil_id, matiere, niveau) VALUES (?, ?, ?)', 
                 [integer, varchar, integer], 
                 Statement),
    odbc_execute(Statement, [Profil, Matiere, Niveau]),
    odbc_free_statement(Statement),
    save_matieres(Profil, Reste).  % Récursivité


save_interets(_, []) :- !.  % Fin de la liste
save_interets(Profil, [InteretNiveau | Reste]) :-
    split_string(InteretNiveau, "-", "", [InteretStr, NiveauStr]), % Séparer "interet-niveau"
    atom_string(Interet, InteretStr),  % Convertir en atom
    number_string(Niveau, NiveauStr),  % Convertir en nombre
    connect_db,
    odbc_prepare(expert_db, 
                 'INSERT INTO interets (profil_id, domaine, niveau) VALUES (?, ?, ?)', 
                 [integer, varchar, integer], 
                 Statement),
    odbc_execute(Statement, [Profil, Interet, Niveau]),
    odbc_free_statement(Statement),
    save_interets(Profil, Reste).  % Récursivité


save_traits_personnalite(_, []) :- !.  % Fin de la liste
save_traits_personnalite(Profil, [TraitNiveau | Reste]) :-
    split_string(TraitNiveau, "-", "", [TraitStr, NiveauStr]), % Séparer "trait-niveau"
    atom_string(Trait, TraitStr),  % Convertir en atom
    number_string(Niveau, NiveauStr),  % Convertir en nombre
    connect_db,
    odbc_prepare(expert_db, 
                 'INSERT INTO traits_personnalite (profil_id, trait, niveau) VALUES (?, ?, ?)', 
                 [integer, varchar, integer], 
                 Statement),
    odbc_execute(Statement, [Profil, Trait, Niveau]),
    odbc_free_statement(Statement),
    save_traits_personnalite(Profil, Reste).  % Récursivité


save_profil(Dict) :-
    % Insérer le profil dans la table 'profils'
    save_profil(Dict.nom, Dict.niveau_etude),

    % Récupérer l'ID du profil inséré
    connect_db,
    % Préparer la requête avec un paramètre de type varchar (chaîne de caractères)
    odbc_prepare(expert_db, 'SELECT id FROM profils WHERE nom = ?', [varchar], Statement),
    % Exécuter la requête avec le paramètre Nom
    odbc_execute(Statement, [Dict.nom], row(Profil)),

    % Ajouter les compétences en passant l'ID du profil
    save_competences(Profil, Dict.competences),

    % Ajouter les matières
    save_matieres(Profil, Dict.matieres),

    % Ajouter les intérêts
    save_interets(Profil, Dict.interets),

    % Ajouter les traits de personnalité
    save_traits_personnalite(Profil, Dict.traits).



% Lancer le serveur automatiquement au démarrage
:- initialization(server(8000)).