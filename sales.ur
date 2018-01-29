type school = {Id: int, Organisation: string, Website: url, Comments: string}
table schools : {Id: int, Organisation: string, Website: url, Comments: string}
                  PRIMARY KEY Id
sequence schools_seq
table contacts : {Id: int, School: int, Time: time, Text: string}
                   PRIMARY KEY Id
                   CONSTRAINT School FOREIGN KEY School REFERENCES schools(Id) ON DELETE CASCADE
sequence contacts_seq

val styleTag = <xml>
  <link href="/normalize.css" rel="stylesheet"/>
  <link href="/style.css" rel="stylesheet"/>
</xml>

fun main () =
    allSchools <- queryX1' (SELECT * FROM schools ORDER BY Schools.Organisation) renderSchoolRow;
    return <xml>
      <head>{styleTag} </head>
      <body>
        <table>
          {allSchools}
        </table>
        <a link={editSchool None}>Nieuwe school</a>
      </body>
    </xml>

and renderSchoolRow (s: school) =
    lastContact <- oneOrNoRows1 (SELECT * FROM contacts WHERE Contacts.School = {[s.Id]} ORDER BY Contacts.Time ASC);
    return <xml>
  <tr>
    <td>{[s.Organisation]}</td>
    <td><a href={s.Website}>{[s.Website]}</a></td>
    <td>{[s.Comments]}</td>
    <td><a link={editSchool (Some s.Id)}>Editeer School</a></td>
    <td>
      {case lastContact of
         None => <xml>Nog geen contact gehad</xml>
       | Some c => <xml><span>{[c.Time]}</span></xml>}
    </td>
    <td>
      {case lastContact of
         None => <xml></xml>
       | Some c => <xml><span>{[c.Text]}</span></xml>}
    </td>
    <td><a link={viewContacts s.Id}>Zie all contacten</a></td>
    <td><a link={addContact s.Id}>Voeg contact toe</a></td>
  </tr>
</xml>

and editSchool (oId : option int) =
    existingSchool <- (case oId of
                         None => return None
                       | Some id => Monad.mp Some (oneRow1 (SELECT *
                                                            FROM schools
                                                            WHERE Schools.Id = {[id]})));
    school <- return (Option.get {Organisation = "", Website = "", Comments = ""}
                                 (Option.mp (fn s => s -- #Id -- #Website ++ {Website = show s.Website}) existingSchool));
    return <xml>
      <head>{styleTag} </head>
      <body>
        <form>
          <label>
            Naam:
            <textbox {#Organisation} value={school.Organisation}/>
          </label>
          <label>
            Website:
            <url_ {#Website} value={school.Website}/>
          </label>
          <label>
            Comments:
            <textarea {#Comments} rows=5>{[school.Comments]}</textarea>
          </label>
          <submit value="Opslaan" action={saveSchool (Option.mp (fn s => s.Id) existingSchool)}></submit>
        </form>
      </body>
    </xml>

and saveSchool (oId : option int) (r: {Organisation: string, Website: string, Comments: string}) =
    (case oId of
       None =>
       id <- nextval schools_seq;
       dml (INSERT INTO schools
              (Id, Organisation, Website, Comments)
            VALUES
              ({[id]}, {[r.Organisation]}, {[bless r.Website]}, {[r.Comments]}))
     | Some id => dml (UPDATE schools
                       SET Organisation = {[r.Organisation]}
                         , Website = {[bless r.Website]}
                         , Comments = {[r.Comments]}
                       WHERE Id = {[id]}));
    redirect (url (main ()))

and viewContacts (id: int) =
    contacts <- queryX1 (SELECT * FROM contacts
                                  WHERE Contacts.School = {[id]}) renderContactRow;
    return <xml>
      <head>{styleTag}</head>
      <body>
        <table>
          {contacts}
        </table>
      </body>
    </xml>
and renderContactRow r = <xml>
  <tr>
    <td>{[r.Time]}</td>
    <td>{[r.Text]}</td>
  </tr>
</xml>

and addContact (id: int) = return <xml>
  <head>{styleTag}</head>
  <body>
    <form>
      <label>
        Contact:
        <textarea {#Text}></textarea>
      </label>
      <submit action={saveContact id} value="Contact opslaan"></submit>
    </form>
  </body>
</xml>

and saveContact schoolId r =
    id <- nextval contacts_seq;
    dml (INSERT INTO contacts
           (Id, School, Time, Text)
         VALUES
           ({[id]}, {[schoolId]}, CURRENT_TIMESTAMP, {[r.Text]}));
    redirect (url (main ()))
