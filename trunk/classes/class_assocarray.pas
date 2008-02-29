{
 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************
Author: Sven Lorenz / Borg@Sven-of-Nine.de
}

unit class_assocarray;

interface
uses Classes,unit_typedefs;

type
  TAssocStringArray = class
   protected
            slKeys   :  TStringList;
            slValues :  TStringList;
            bOK      :  Boolean;
            function    GetString(Key : LongString): LongString;
            procedure   SetString(Key : LongString;  Value: Longstring);
   public
            constructor Create();
            destructor  Destroy(); override;

            property  Strings[Key: LongString]: String read GetString write SetString; default;
            procedure Delete(Key: LongString);
            function  Count():unsigned32;
            procedure Clear;

            property  OK : boolean read bOK;
  end;


type
  TAssocIntegerArray = class
   protected
            slKeys   :  TStringList;
            bOK      :  Boolean;
            function    GetInteger(Key : LongString): Signed32;
            procedure   SetInteger(Key : LongString;  Value: signed32);
   public
            constructor Create();
            destructor  Destroy(); override;

            property  Strings[Key: LongString]: signed32 read GetInteger write SetInteger; default;
            procedure Delete(Key: LongString);
            function  Count():unsigned32;
            procedure Clear;

            property  OK : boolean read bOK;
  end;

implementation

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
///
/// Implementierung des Stringarrays
///
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////////
/// Konstruktor / Destruktor
////////////////////////////////////////////////////////////////////////////////
constructor TAssocStringArray.Create();
begin
     //Meine Stringlisten initialisieren
     slKeys   := TStringList.Create;
     slValues := TStringList.Create;
     //Fehler setzen
     bOK      :=FALSE
end;

destructor TAssocStringArray.Destroy();
begin
     //Alles freimachen
     slKeys.Free;
     slValues.Free;
end;


////////////////////////////////////////////////////////////////////////////////
/// Properties
////////////////////////////////////////////////////////////////////////////////
//Einen String holen
function TAssocStringArray.GetString(Key: LongString): LongString;
var
   s32Index: Integer;
begin
     result:='';
     //Eintrag suchen
     s32Index:= slKeys.IndexOf(Key);
     if (s32Index<>-1) then
        begin
             Result := slValues[s32Index];
             bOK:=TRUE;
        end
     else
        begin
             bOK:=FALSE;
        end;
end;

////////////////////////////////////////////////////////////////////////////////
//Einen String setzen
procedure TAssocStringArray.SetString(Key: String; Value: String);
var
   s32Index : signed32;
begin
     //Gibt es schon einen Eintrag ?
     s32Index := slKeys.IndexOf(Key);
     if (s32Index<>-1) then
        begin
             //Ja, dann neu belegen
             slValues[s32Index] := Value;
        end
     else
        begin
             //Nein, dann hinzufügen
             slKeys.Add(Key);
             slValues.Add(Value);
        end;
     bOK:=TRUE;
end;

////////////////////////////////////////////////////////////////////////////////
//Einen String löschen
procedure TAssocStringArray.Delete(Key: String);
var
   s32Index : signed32;
begin
     //Existiert der Eintrag ?
     s32Index := slKeys.IndexOf(Key);
     if (s32Index <> -1) then
        begin
             //Ja, dann aus beiden listen löschen
             slKeys.Delete(s32Index);
             slValues.Delete(s32Index);
        end;
     bOK:=TRUE;
end;

////////////////////////////////////////////////////////////////////////////////
//Die Liste löschen
procedure TAssocStringArray.Clear;
begin
     slKeys.Clear;
     slValues.Clear;
end;

////////////////////////////////////////////////////////////////////////////////
//Anzahl der Einträge lesen
function  TAssocStringArray.Count():unsigned32;
begin
     result:=slKeys.Count;
end;


////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
///
/// Implementierung des Integerarrays
///
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////////
/// Konstruktor / Destruktor
////////////////////////////////////////////////////////////////////////////////
constructor TAssocIntegerArray.Create();
begin
     //Meine Stringlisten initialisieren
     slKeys   := TStringList.Create;
end;

destructor TAssocIntegerArray.Destroy();
begin
     //Alles freimachen
     slKeys.Free;
end;


////////////////////////////////////////////////////////////////////////////////
/// Properties
////////////////////////////////////////////////////////////////////////////////
//Eine Integer holen
function TAssocIntegerArray.GetInteger(Key: LongString): signed32;
var
   s32Index: Integer;
begin
     //Fehler annehmen
     result:=0;
     //Eintrag suchen
     s32Index:= slKeys.IndexOf(Key);
     if (s32Index<>-1) then
        begin
             Result := signed32(slKeys.Objects[s32Index]);
             bOK:=TRUE;
        end
     else
        begin
             bOK:=FALSE;
        end;
end;

////////////////////////////////////////////////////////////////////////////////
//Einen String setzen
procedure TAssocIntegerArray.SetInteger(Key: String; Value: signed32);
var
   s32Index : signed32;
begin
     //Gibt es schon einen Eintrag ?
     s32Index := slKeys.IndexOf(Key);
     if (s32Index<>-1) then
        begin
             //Ja, dann neu belegen
             slKeys.Objects[s32Index] := TObject(Value);
        end
     else
        begin
             //Nein, dann hinzufügen
             slKeys.AddObject(Key,TObject(Value));
        end;
     bOK:=TRUE;
end;

////////////////////////////////////////////////////////////////////////////////
//Einen String löschen
procedure TAssocIntegerArray.Delete(Key: String);
var
   s32Index : signed32;
begin
     //Existiert der Eintrag ?
     s32Index := slKeys.IndexOf(Key);
     if (s32Index <> -1) then
        begin
             //Ja, dann aus beiden listen löschen
             slKeys.Delete(s32Index);
        end;
     bOK:=TRUE;
end;

////////////////////////////////////////////////////////////////////////////////
//Die Liste löschen
procedure TAssocIntegerArray.Clear;
begin
     slKeys.Clear;
end;

////////////////////////////////////////////////////////////////////////////////
//Anzahl der Einträge lesen
function  TAssocIntegerArray.Count():unsigned32;
begin
     result:=slKeys.Count;
end;


end.
