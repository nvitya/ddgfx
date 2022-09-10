unit formMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ddgfx;


type

  { TfrmMain }

  TfrmMain = class(TForm)
    btnExit : TButton;
    oglpanel : TPanel;
    pnl : TPanel;
    procedure btnExitClick(Sender : TObject);
    procedure FormCreate(Sender : TObject);
    procedure oglpanelClick(Sender : TObject);
  private

  public
    scene : TddScene;

    shp : TShape;

    procedure OglboxResize(Sender: TObject);

  end;

var
  frmMain : TfrmMain;

implementation

uses dglOpenGL;

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.btnExitClick(Sender : TObject);
begin
  Application.Terminate;
end;

procedure TfrmMain.FormCreate(Sender : TObject);
const
  triangle_verts : array[0..2] of TVertex = ((0, 0), (1, 0), (1, 1));
var
  csh : TClonedShape;
  grp : TDrawGroup;
  cgr : TClonedGroup;
begin
  scene := TddScene.Create(self, oglpanel);

  shp := TShape.Create(nil);
  shp.AddPrimitive(GL_TRIANGLES, 3, @triangle_verts[0]);
  shp.x := 100;
  shp.y := 100;
  shp.scalex := 100;
  shp.scaley := 100;
  shp.alpha := 0.5;
  shp.rotation := 00;
  shp.SetColor(1, 0, 0, 1);

  grp := TDrawGroup.Create(scene.root);

  csh := TClonedShape.Create(grp, shp);

  csh := TClonedShape.Create(grp, shp);
  csh.x += 50;
  csh.rotation := 20;
  csh.SetColor(0, 1, 0, 0.4);

  csh := TClonedShape.Create(grp, shp);
  csh.x += 70;
  csh.rotation := 30;
  csh.SetColor(0, 0, 1, 0.4);

  cgr := TClonedGroup.Create(scene.root, grp);
  cgr.x += 100;
  cgr.y += 50;
  cgr.alpha := 0.5;


  //scene.bgcolor.r := 1;

  scene.OnResize := @OglboxResize;
end;

procedure TfrmMain.oglpanelClick(Sender : TObject);
begin

end;

procedure TfrmMain.OglboxResize(Sender : TObject);
begin

end;


end.

