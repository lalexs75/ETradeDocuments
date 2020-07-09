{ interface library for FPC and Lazarus

  Copyright (C) 2019 Lagunov Aleksey alexs75@yandex.ru

  Генерация xml файлов для электронного документооборота
  Формат заявления о ввозе товаров и уплате косвенных налогов российского налогоплательщика

  Структуры данных базируются на основании "Приказ от 19.11.2014 № ММВ-7-6/590@"

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

unit ImportGoodsAndIndirectTaxesExchangeFile;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, xmlobject, AbstractExchangeFileUnit, ImportGoodsAndIndirectTaxesDocument,
  AbstractSerializationObjects;

type

  { TImportGoodsAndIndirectTaxesExchangeFile }

  TImportGoodsAndIndirectTaxesExchangeFile = class(TAbstractExchangeFile)   //%Таблица 4.1
  private
    FDocument: TImportGoodsAndIndirectTaxesDocument;
    FInformationType: string;
    FRecipientTaxInspectionCode: string;
    procedure SetInformationType(AValue: string);
    procedure SetRecipientTaxInspectionCode(AValue: string);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
    property InformationType:string read FInformationType write SetInformationType;
    property RecipientTaxInspectionCode:string read FRecipientTaxInspectionCode write SetRecipientTaxInspectionCode;
    property Document:TImportGoodsAndIndirectTaxesDocument read FDocument;
  end;

implementation

{ TImportGoodsAndIndirectTaxesExchangeFile }
(*
procedure TImportGoodsAndIndirectTaxesExchangeFile.SetAppVersion(AValue: string
  );
begin
  if FAppVersion=AValue then Exit;
  FAppVersion:=AValue;
  ModifiedProperty('AppVersion');
end;

procedure TImportGoodsAndIndirectTaxesExchangeFile.SetFileID(AValue: string);
begin
  if FFileID=AValue then Exit;
  FFileID:=AValue;
  ModifiedProperty('FileID');
end;

procedure TImportGoodsAndIndirectTaxesExchangeFile.SetFormatVersion(
  AValue: string);
begin
  if FFormatVersion=AValue then Exit;
  FFormatVersion:=AValue;
  ModifiedProperty('FormatVersion');
end;
*)
procedure TImportGoodsAndIndirectTaxesExchangeFile.SetInformationType(
  AValue: string);
begin
  if FInformationType=AValue then Exit;
  FInformationType:=AValue;
  ModifiedProperty('InformationType');
end;

procedure TImportGoodsAndIndirectTaxesExchangeFile.SetRecipientTaxInspectionCode
  (AValue: string);
begin
  if FRecipientTaxInspectionCode=AValue then Exit;
  FRecipientTaxInspectionCode:=AValue;
  ModifiedProperty('RecipientTaxInspectionCode');
end;

procedure TImportGoodsAndIndirectTaxesExchangeFile.InternalRegisterPropertys;
begin
  inherited InternalRegisterPropertys;
  RegisterProperty('InformationType', 'ТипИнф', [xsaRequared], 'Тип информации', 1, 50);
  RegisterProperty('RecipientTaxInspectionCode', 'КодНО', [xsaRequared], 'Код налогового органа получателя', 1, 4);
  RegisterProperty('Document', 'Документ', [xsaRequared], 'Сведения заявления российского налогоплательщика о ввозе товаров и уплате косвенных налогов', -1, -1);
end;

procedure TImportGoodsAndIndirectTaxesExchangeFile.InternalInitChilds;
begin
  inherited InternalInitChilds;
  FDocument:=TImportGoodsAndIndirectTaxesDocument.Create;
end;

destructor TImportGoodsAndIndirectTaxesExchangeFile.Destroy;
begin
  FreeAndNil(FDocument);
  inherited Destroy;
end;

end.

