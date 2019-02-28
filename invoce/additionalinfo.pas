{ interface library for FPC and Lazarus

  Copyright (C) 2019 Lagunov Aleksey alexs75@yandex.ru

  Генерация xml файлов для электронного документооборота

  Структуры данных базируются на основании "Приказ ФНС РФ от 19.12.2018 N ММВ-7-15/820@"

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

unit AdditionalInfo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, xml_doc;

type

  { TTextInfo }

  TTextInfo = class(TXmlSerializationObject)   //%Таблица 5.47
  private
    FID: string;
    FValue: string;
    procedure SetID(AValue: string);
    procedure SetValue(AValue: string);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
    property ID:string read FID write SetID;
    property Value:string read FValue write SetValue;
  end;

  { TTextInfoList }

  TTextInfoList = class(TXmlSerializationObjectList) //%Таблица 5.47
  private
    function GetItem(AIndex: Integer): TTextInfo; inline;
  public
    constructor Create;
    function CreateChild:TTextInfo;
    property Item[AIndex:Integer]:TTextInfo read GetItem; default;
  end;

  { TAdditionalInfo1 }

  TAdditionalInfo1 = class(TXmlSerializationObject)   //%Таблица 5.12
  private
    FInfoFileID: string;
    FTextInfo: TTextInfoList;
    procedure SetInfoFileID(AValue: string);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
    property InfoFileID:string read FInfoFileID write SetInfoFileID;
    property TextInfo:TTextInfoList read FTextInfo;
  end;

  { TAdditionalInfo3 }

  TAdditionalInfo3 = class(TXmlSerializationObject)   //%Таблица 5.30
  private
    FInfoFileID: string;
    FTextInfo: TTextInfoList;
    procedure SetInfoFileID(AValue: string);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
    property InfoFileID:string read FInfoFileID write SetInfoFileID;
    property TextInfo:TTextInfoList read FTextInfo;
  end;

implementation

{ TAdditionalInfo3 }

procedure TAdditionalInfo3.SetInfoFileID(AValue: string);
begin
  if FInfoFileID=AValue then Exit;
  FInfoFileID:=AValue;
  ModifiedProperty('InfoFileID');
end;

procedure TAdditionalInfo3.InternalRegisterPropertys;
begin
  RegisterProperty('InfoFileID', 'ИдФайлИнфПол', 'Н', 'Идентификатор файла информационного поля', 36, 36);
  RegisterProperty('TextInfo', 'ТекстИнф', 'НМ', 'Текстовая информация', -1, -1);
end;

procedure TAdditionalInfo3.InternalInitChilds;
begin
  inherited InternalInitChilds;
  FTextInfo:=TTextInfoList.Create;
end;

destructor TAdditionalInfo3.Destroy;
begin
  FreeAndNil(FTextInfo);
  inherited Destroy;
end;

{ TTextInfoList }

function TTextInfoList.GetItem(AIndex: Integer): TTextInfo;
begin
  Result:=TTextInfo(InternalGetItem(AIndex));
end;

constructor TTextInfoList.Create;
begin
  inherited Create(TTextInfo)
end;

function TTextInfoList.CreateChild: TTextInfo;
begin
  Result:=InternalAddObject as TTextInfo;
end;

{ TTextInfo }

procedure TTextInfo.SetID(AValue: string);
begin
  if FID=AValue then Exit;
  FID:=AValue;
  ModifiedProperty('ID');
end;

procedure TTextInfo.SetValue(AValue: string);
begin
  if FValue=AValue then Exit;
  FValue:=AValue;
  ModifiedProperty('Value');
end;

procedure TTextInfo.InternalRegisterPropertys;
begin
  RegisterProperty('ID', 'Идентиф', 'О', 'Идентификатор', 1, 50);
  RegisterProperty('Value', 'Значен', 'О', 'ИЗначение', 1, 2000);
end;

procedure TTextInfo.InternalInitChilds;
begin
  inherited InternalInitChilds;
end;

destructor TTextInfo.Destroy;
begin
  inherited Destroy;
end;

{ TAdditionalInfo1 }

procedure TAdditionalInfo1.SetInfoFileID(AValue: string);
begin
  if FInfoFileID=AValue then Exit;
  FInfoFileID:=AValue;
  ModifiedProperty('InfoFileID');
end;

procedure TAdditionalInfo1.InternalRegisterPropertys;
begin
  RegisterProperty('InfoFileID', 'ИдФайлИнфПол', 'Н', 'Идентификатор файла информационного поля', 36, 36);
  RegisterProperty('TextInfo', 'ТекстИнфТип', 'НМ', 'Текстовая информация', -1, -1);
end;

procedure TAdditionalInfo1.InternalInitChilds;
begin
  inherited InternalInitChilds;
  FTextInfo:=TTextInfoList.Create;
end;

destructor TAdditionalInfo1.Destroy;
begin
  FreeAndNil(FTextInfo);
  inherited Destroy;
end;

end.

