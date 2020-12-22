{ GS1 interface library for FPC and Lazarus

  Copyright (C) 2020 Lagunov Aleksey alexs75@yandex.ru

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

unit crpt_participants_info;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, JSONObjects, AbstractSerializationObjects;

type

  { TParticipantsInfo }

  TParticipantsInfo = class(TJSONSerializationObject)
  private
    FactualAddress: string;
    Fdissolved: boolean;
    Femail: string;
    FemissionRegistrars: string;
    FfullName: string;
    Fhead: TXSDStringArray;
    Fid: Int64;
    Finn: string;
    Fkpp: string;
    FlegalAddress: string;
    Fname: string;
    Fogrn: string;
    ForganizationForm: string;
    Fphone: string;
    FregistrationDate: Int64;
    Froles: TXSDStringArray;
    FshortName: string;
    Fstatus: string;
    Ftypes1: string;
    procedure SetactualAddress(AValue: string);
    procedure Setdissolved(AValue: boolean);
    procedure Setemail(AValue: string);
    procedure SetemissionRegistrars(AValue: string);
    procedure SetfullName(AValue: string);
    procedure Sethead(AValue: TXSDStringArray);
    procedure Setid(AValue: Int64);
    procedure Setinn(AValue: string);
    procedure Setkpp(AValue: string);
    procedure SetlegalAddress(AValue: string);
    procedure Setname(AValue: string);
    procedure Setogrn(AValue: string);
    procedure SetorganizationForm(AValue: string);
    procedure Setphone(AValue: string);
    procedure SetregistrationDate(AValue: Int64);
    procedure Setroles(AValue: TXSDStringArray);
    procedure SetshortName(AValue: string);
    procedure Setstatus(AValue: string);
    procedure Settypes1(AValue: string);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  public
    constructor Create; override;
  published
    property id:Int64 read Fid write Setid; //Идентификатор
    property inn:string read Finn write Setinn; //ИНН
    property name:string read Fname write Setname; //Наименование
    property shortName:string read FshortName write SetshortName; //Краткое наименование
    property fullName:string read FfullName write SetfullName; //Полное наименование
    property types1:string read Ftypes1 write Settypes1; //Тип организации
    property status:string read Fstatus write Setstatus; //Статус REGISTERED -Зарегистрирован;NOT_REGISTERED - Не зарегистрирован;REMOVED - Удален;RESTORED - Восстановлен;BLOCKED - Заблокирован
    property kpp:string read Fkpp write Setkpp; //КПП
    property ogrn:string read Fogrn write Setogrn; //ОГРН
    property head:TXSDStringArray read Fhead write Sethead; //Руководители
    property legalAddress:string read FlegalAddress write SetlegalAddress; //Юридический адрес
    property actualAddress:string read FactualAddress write SetactualAddress; //Фактический адрес
    property email:string read Femail write Setemail; //Email
    property registrationDate:Int64 read FregistrationDate write SetregistrationDate; //Регистрация
    property organizationForm:string read ForganizationForm write SetorganizationForm; //Форма организации: ЮЛ, ФЛ, ИП
    property emissionRegistrars:string read FemissionRegistrars write SetemissionRegistrars; //Регистратор эмиссии
    property dissolved:boolean read Fdissolved write Setdissolved; //Признак существования организации
    property roles:TXSDStringArray read Froles write Setroles; //Роли организации
    property phone:string read Fphone write Setphone; //Телефон
//    property productGroupInfo:array-Тип участникаВозвращается при использовании токена администратора
  end;

implementation

{ TParticipantsInfo }

procedure TParticipantsInfo.SetactualAddress(AValue: string);
begin
  if FactualAddress=AValue then Exit;
  FactualAddress:=AValue;
  ModifiedProperty('actualAddress');
end;

procedure TParticipantsInfo.Setdissolved(AValue: boolean);
begin
  if Fdissolved=AValue then Exit;
  Fdissolved:=AValue;
  ModifiedProperty('dissolved');
end;

procedure TParticipantsInfo.Setemail(AValue: string);
begin
  if Femail=AValue then Exit;
  Femail:=AValue;
  ModifiedProperty('email');
end;

procedure TParticipantsInfo.SetemissionRegistrars(AValue: string);
begin
  if FemissionRegistrars=AValue then Exit;
  FemissionRegistrars:=AValue;
  ModifiedProperty('emissionRegistrars');
end;

procedure TParticipantsInfo.SetfullName(AValue: string);
begin
  if FfullName=AValue then Exit;
  FfullName:=AValue;
  ModifiedProperty('fullName');
end;

procedure TParticipantsInfo.Sethead(AValue: TXSDStringArray);
begin
  if Fhead=AValue then Exit;
  Fhead:=AValue;
  ModifiedProperty('head');
end;

procedure TParticipantsInfo.Setid(AValue: Int64);
begin
  if Fid=AValue then Exit;
  Fid:=AValue;
  ModifiedProperty('id');
end;

procedure TParticipantsInfo.Setinn(AValue: string);
begin
  if Finn=AValue then Exit;
  Finn:=AValue;
  ModifiedProperty('inn');
end;

procedure TParticipantsInfo.Setkpp(AValue: string);
begin
  if Fkpp=AValue then Exit;
  Fkpp:=AValue;
  ModifiedProperty('kpp');
end;

procedure TParticipantsInfo.SetlegalAddress(AValue: string);
begin
  if FlegalAddress=AValue then Exit;
  FlegalAddress:=AValue;
  ModifiedProperty('legalAddress');
end;

procedure TParticipantsInfo.Setname(AValue: string);
begin
  if Fname=AValue then Exit;
  Fname:=AValue;
  ModifiedProperty('name');
end;

procedure TParticipantsInfo.Setogrn(AValue: string);
begin
  if Fogrn=AValue then Exit;
  Fogrn:=AValue;
  ModifiedProperty('ogrn');
end;

procedure TParticipantsInfo.SetorganizationForm(AValue: string);
begin
  if ForganizationForm=AValue then Exit;
  ForganizationForm:=AValue;
  ModifiedProperty('organizationForm');
end;

procedure TParticipantsInfo.Setphone(AValue: string);
begin
  if Fphone=AValue then Exit;
  Fphone:=AValue;
  ModifiedProperty('phone');
end;

procedure TParticipantsInfo.SetregistrationDate(AValue: Int64);
begin
  if FregistrationDate=AValue then Exit;
  FregistrationDate:=AValue;
  ModifiedProperty('registrationDate');
end;

procedure TParticipantsInfo.Setroles(AValue: TXSDStringArray);
begin
  if Froles=AValue then Exit;
  Froles:=AValue;
  ModifiedProperty('roles');
end;

procedure TParticipantsInfo.SetshortName(AValue: string);
begin
  if FshortName=AValue then Exit;
  FshortName:=AValue;
  ModifiedProperty('shortName');
end;

procedure TParticipantsInfo.Setstatus(AValue: string);
begin
  if Fstatus=AValue then Exit;
  Fstatus:=AValue;
  ModifiedProperty('status');
end;

procedure TParticipantsInfo.Settypes1(AValue: string);
begin
  if Ftypes1=AValue then Exit;
  Ftypes1:=AValue;
  ModifiedProperty('types1');
end;

procedure TParticipantsInfo.InternalRegisterPropertys;
begin
  inherited InternalRegisterPropertys;
  RegisterProperty('id', 'id', [], '', -1, -1);
  RegisterProperty('inn', 'inn', [], '', -1, -1);
  RegisterProperty('name', 'name', [], '', -1, -1);
  RegisterProperty('shortName', 'shortName', [], '', -1, -1);
  RegisterProperty('fullName', 'fullName', [], '', -1, -1);
  RegisterProperty('types1', 'types', [], '', -1, -1);
  RegisterProperty('status', 'status', [], '', -1, -1);
  RegisterProperty('kpp', 'kpp', [], '', -1, -1);
  RegisterProperty('ogrn', 'ogrn', [], '', -1, -1);
  RegisterProperty('head', 'head', [], '', -1, -1);
  RegisterProperty('legalAddress', 'legalAddress', [], '', -1, -1);
  RegisterProperty('actualAddress', 'actualAddress', [], '', -1, -1);
  RegisterProperty('email', 'email', [], '', -1, -1);
  RegisterProperty('registrationDate', 'registrationDate', [], '', -1, -1);
  RegisterProperty('organizationForm', 'organizationForm', [], '', -1, -1);
  RegisterProperty('emissionRegistrars', 'emissionRegistrars', [], '', -1, -1);
  RegisterProperty('dissolved', 'dissolved', [], '', -1, -1);
  RegisterProperty('roles', 'roles', [], '', -1, -1);
  RegisterProperty('phone', 'phone', [], '', -1, -1);
//    property productGroupInfo:array-Тип участникаВозвращается при использовании токена администратора
end;

procedure TParticipantsInfo.InternalInitChilds;
begin
  inherited InternalInitChilds;
end;

destructor TParticipantsInfo.Destroy;
begin
  inherited Destroy;
end;

constructor TParticipantsInfo.Create;
begin
  inherited Create;
  FIgnoreReadUndefProps:=true;
end;

end.

