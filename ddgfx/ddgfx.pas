(* -----------------------------------------------------------------------------
 * This file is a part of the ddgfx project: https://github.com/nvitya/ddgfx
 * Copyright (c) 2022 Viktor Nagy, nvitya
 *
 * This software is provided 'as-is', without any express or implied warranty.
 * In no event will the authors be held liable for any damages arising from
 * the use of this software. Permission is granted to anyone to use this
 * software for any purpose, including commercial applications, and to alter
 * it and redistribute it freely, subject to the following restrictions:
 *
 * 1. The origin of this software must not be misrepresented; you must not
 *    claim that you wrote the original software. If you use this software in
 *    a product, an acknowledgment in the product documentation would be
 *    appreciated but is not required.
 *
 * 2. Altered source versions must be plainly marked as such, and must not be
 *    misrepresented as being the original software.
 *
 * 3. This notice may not be removed or altered from any source distribution.
 * --------------------------------------------------------------------------- */
 *  file:     ddgfx.pas
 *  brief:    ddGfx main unit
 *  date:     2022-09-09
 *  authors:  nvitya
*)

unit ddgfx;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Controls, dglOpenGL, OpenGLContext;

type
  TddFloat    = TGLfloat;
  TddMatrix   = array[0..5] of TddFloat;  // we leave out the trivial values from this
  TddMatrixGL = array[0..8] of TddFloat;  // GL requires the full matrix format

  TddColorUint = longword;
  TddColorGL = packed record
    r : TddFloat;
    g : TddFloat;
    b : TddFloat;
    a : TddFloat;
  end;


type
  TddVertex = array[0..1] of TddFloat;
  PddVertex = ^TddVertex;

type
  // short aliases
  TMatrix = TddMatrix;
  TMatrixGL = TddMatrixGL;
  TVertex = TddVertex;
  PVertex = PddVertex;
  TColorGL = TddColorGL;
  TColorUint = TddColorUint;

type

  { TPrimitive }

  TPrimitive = class // a primitive that can be drawn by the OpenGL directly
  public
    drawmode  : GLint;  // GL_TRIANGLE_STRIP or GL_TRIANGLE_FAN
    vertices  : array of TVertex;
    vertexcount : integer;

    constructor Create(amode : GLint; avertexcount : integer; vertdata : PVertex);
    destructor Destroy; override;

    procedure Draw();

  protected
    vao      : TGLint;  // vertex array object
    vbo      : TGLint;  // vertex buffer object

    glbuf    : TGLint;
    bufferok : boolean;

    procedure UpdateBuffer;
  end;

  { TShaderProgram }

  TShaderType = (stVertex, stFragment);

  TShaderProgram = class
  protected
    FPrgHandle : TGLint;
    FVSHandle : TGLint;
    FFSHandle : TGLint;

    procedure CompileShader(shadertype : TShaderType; const src : string);
    procedure LinkProgram;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure Activate;

    property PrgHandle : TGLint read FPrgHandle;
  end;

  { TSimpleShader }

  TSimpleShader = class(TShaderProgram)
  private
    Fu_MVPMatrix : TGLint;
    Fu_Color : TGLint;
  public
    constructor Create; override;

    procedure SetMVPMatrix(const mat : TMatrix);
    procedure SetColor(r,g,b,a : single); overload;
    procedure SetColor(const glc : TColorGL); overload;
  end;

  { TDrawable }

  TDrawGroup = class;

  TDrawable = class  // base of all the visual classes
  public
    parent   : TDrawGroup;

    x        : TddFloat;
    y        : TddFloat;
    scalex   : TddFloat;
    scaley   : TddFloat;
    rotation : TddFloat;
    alpha    : TddFloat;
    matrix   : TMatrix;
    visible  : boolean;

    constructor Create(aparent : TDrawGroup); virtual;
    destructor Destroy; override;

    procedure Draw(const apmatrix : TMatrix; aalpha : TddFloat); virtual; abstract;

    procedure CopyProperties(acopyfrom : TDrawable);

    procedure UpdateMatrix();

  end;

  { TShape }

  TShape = class(TDrawable)
  public
    color : TColorGL;  // white by default
    primitives : array of TPrimitive;

    constructor Create(aparent : TDrawGroup); override;
    destructor Destroy; override;

    function AddPrimitive(amode : GLint; avertcount : integer; avertdata : PVertex) : TPrimitive;
    procedure Clear(); // removes all primitives

    procedure SetColor(r, g, b : TddFloat); overload;
    procedure SetColor(r, g, b, a : TddFloat); overload;

    procedure Draw(const apmatrix : TMatrix; apalpha : TddFloat); override;
  end;

  { TClonedShape }

  TClonedShape = class(TDrawable)
  public
    original : TShape;
    color : TColorGL;  // white by default, overrides the originals color

    constructor Create(aparent : TDrawGroup; aoriginal : TShape); reintroduce;
    destructor Destroy; override;

    procedure SetColor(r, g, b : TddFloat); overload;
    procedure SetColor(r, g, b, a : TddFloat); overload;

    procedure Draw(const apmatrix : TMatrix; apalpha : TddFloat); override;
  end;


  { TDrawGroup }

  TDrawGroup = class(TDrawable)
  public
    children : array of TDrawable;

    constructor Create(aparent : TDrawGroup); override;
    destructor Destroy; override;

    procedure AddChild(adr : TDrawable);

    procedure Draw(const apmatrix : TMatrix; apalpha : TddFloat); override;

  end;

  { TClonedGroup }

  TClonedGroup = class(TDrawable)
  public
    original : TDrawGroup;

    constructor Create(aparent : TDrawGroup; aoriginal : TDrawGroup); reintroduce;
    destructor Destroy; override;

    procedure Draw(const apmatrix : TMatrix; apalpha : TddFloat); override;
  end;


  { TddTester }

  TddTester = class
  public
    vertices : array[0..2] of TVertex;
    vao : TGLInt;
    vbo : TGLInt;

    constructor Create;

    procedure Draw;
  end;

  { TddScene }

  TddScene = class(TOpenGLControl) // scene root, add to the main window
  public

    tst : TddTester;

    bgcolor : TColorGL;

    root : TDrawGroup;

    mat_projection : TMatrix;

    constructor Create(aowner : TComponent; aparent : TWinControl); reintroduce;
    destructor Destroy; override;

    procedure DoOnPaint; override;

  protected
    procedure DoOnResize; override;

    procedure SetViewPort(awidth, aheight : integer);

  end;



const
  ShaderTypeName : array[TShaderType] of string = ('vertex', 'fragment');

procedure ddmat_identity(out mat : TMatrix);
procedure ddmat_mul(out mat : TMatrix; const mat1, mat2 : TMatrix); overload; // ensure that mat <> mat1 !!!
procedure ddmat_mul(var mat : TMatrix; const mat2 : TMatrix); overload;

procedure ddmat_scale(var mat : TMatrix; const xscale, yscale : single);
procedure ddmat_translate(var mat : TMatrix; const x, y : single);
procedure ddmat_rotate(var mat : TMatrix; const alpha : single);

procedure ddmat_to_gl(out matout : TMatrixGL; const matin : TMatrix);
procedure ddmat_set_projection(out projmat : TMatrix; awidth, aheight : single);

var
  activeshader : TShaderProgram;
  fixcolorshader : TSimpleShader;

implementation

const
  attrloc_position = 0;
  //attrloc_texcoordinate = 1;

procedure ddmat_identity(out mat : TMatrix);
begin
  mat[0] := 1;
  mat[1] := 0;
  mat[2] := 0;
  mat[3] := 1;
  mat[4] := 0;
  mat[5] := 0;
end;

procedure ddmat_mul(out mat : TMatrix; const mat1, mat2 : TMatrix); // ensure that mat <> mat1 !!!
begin
  mat[0] := mat1[0]*mat2[0] + mat1[1]*mat2[2];
  mat[1] := mat1[0]*mat2[1] + mat1[1]*mat2[3];

  mat[2] := mat1[2]*mat2[0] + mat1[3]*mat2[2];
  mat[3] := mat1[2]*mat2[1] + mat1[3]*mat2[3];

  mat[4] := mat1[4]*mat2[0] + mat1[5]*mat2[2] + mat2[4];
  mat[5] := mat1[4]*mat2[1] + mat1[5]*mat2[3] + mat2[5];
end;

procedure ddmat_mul(var mat : TMatrix; const mat2 : TMatrix);
var
  m0, m2, m4 : single;
begin
  m0 := mat[0];
  mat[0] := m0*mat2[0] + mat[1]*mat2[2];
  mat[1] := m0*mat2[1] + mat[1]*mat2[3];

  m2 := mat[2];
  mat[2] := m2*mat2[0] + mat[3]*mat2[2];
  mat[3] := m2*mat2[1] + mat[3]*mat2[3];

  m4 := mat[4];
  mat[4] := m4*mat2[0] + mat[5]*mat2[2] + mat2[4];
  mat[5] := m4*mat2[1] + mat[5]*mat2[3] + mat2[5];
end;

procedure ddmat_scale(var mat : TMatrix; const xscale, yscale : single);
begin
  mat[0] := mat[0]*xscale;
  mat[1] := mat[1]*yscale;
  mat[2] := mat[2]*xscale;
  mat[3] := mat[3]*yscale;
  mat[4] := mat[4]*xscale;
  mat[5] := mat[5]*yscale;
end;

procedure ddmat_translate(var mat : TMatrix; const x, y : single);
begin
  mat[4] := mat[4] + x;
  mat[5] := mat[5] + y;
end;

procedure ddmat_rotate(var mat : TMatrix; const alpha : single);
var
  cosa : single;
  sina : single;
  m0, m2, m4 : single;
begin
  cosa := cos(alpha);
  sina := sin(alpha);

  m0 := mat[0];
  mat[0] := m0*cosa + mat[1]*sina;
  mat[1] := m0*(-sina) + mat[1]*cosa;

  m2 := mat[2];
  mat[2] := m2*cosa + mat[3]*sina;
  mat[3] := m2*(-sina) + mat[3]*cosa;

  m4 := mat[4];
  mat[4] := m4*cosa + mat[5]*sina;
  mat[5] := m4*(-sina) + mat[5]*cosa;
end;

procedure ddmat_to_gl(out matout : TMatrixGL; const matin : TMatrix);
begin
  matout[0] := matin[0];
  matout[1] := matin[1];
  matout[2] := 0;

  matout[3] := matin[2];
  matout[4] := matin[3];
  matout[5] := 0;

  matout[6] := matin[4];
  matout[7] := matin[5];
  matout[8] := 1;
end;

procedure ddmat_set_projection(out projmat : TMatrix; awidth, aheight : single);
begin
  ddmat_identity(projmat);

  projmat[0] :=  2 / awidth;
  projmat[3] := -2 / aheight;

  projmat[4] := -1;  // translate x
  projmat[5] :=  1;  // translate y
end;


{ TddTester }

constructor TddTester.Create;
begin
  vertices[0][0] := 0; vertices[0][1] := 0;
  vertices[1][0] := 1; vertices[1][1] := 0;
  vertices[2][0] := 1; vertices[2][1] := 1;

  vao := -1;
  vbo := -1;
end;

procedure TddTester.Draw;
var
  m : TMatrix;
  i : integer;
begin

  ddmat_identity(m);
  //ddmat_scale(m, 0.5, 0.5);
  //ddmat_rotate(m, 30);
  fixcolorshader.SetMVPMatrix(m);
  fixcolorshader.SetColor(1,0,0,0.3);

  if vao < 0 then
  begin
    glGenVertexArrays(1, @vao);
    glGenBuffers(1, @vbo);

    // Daten für das Dreieck
    glBindVertexArray(vao);
    glBindBuffer(GL_ARRAY_BUFFER, vbo);
    i := sizeof(vertices);
    glBufferData(GL_ARRAY_BUFFER, i, @vertices[0], GL_STATIC_DRAW);
    //glBufferData(GL_ARRAY_BUFFER, sizeof(vertices), @vertices[0], GL_STATIC_DRAW);
    glEnableVertexAttribArray(0);
    glVertexAttribPointer(0, 2, GL_FLOAT, False, 0, nil);
  end;

  // Zeichne Dreieck
  glBindVertexArray(vao);
  glDrawArrays(GL_TRIANGLES, 0, 3 * 2);
end;

{ TShaderProgram }

constructor TShaderProgram.Create;
begin
  FVSHandle  := 0;
  FFSHandle  := 0;
  FPrgHandle := glCreateProgram();
  if FPrgHandle = 0 then raise Exception.Create('Could not create shader program');
end;

destructor TShaderProgram.Destroy;
begin
  if FVSHandle <> 0 then glDeleteShader(FVSHandle);
  if FFSHandle <> 0 then glDeleteShader(FFSHandle);
  if FPrgHandle <> 0 then glDeleteProgram(FPrgHandle);

  inherited Destroy;
end;

procedure TShaderProgram.CompileShader(shadertype : TShaderType; const src : string);
var
  i : GLint;
  psrc : PChar;
  len : TGLSizei;
  s : string;
  handle : GLint;
begin
  if shadertype = stVertex then
  begin
    handle := glCreateShader(GL_VERTEX_SHADER);
  end
  else
  begin
    handle := glCreateShader(GL_FRAGMENT_SHADER);
  end;

  if handle <= 0 then raise Exception.Create('Error creating '+ShaderTypeName[shadertype]+' shader');

  psrc := PChar(src);
  len := length(src);
  glShaderSource(handle, 1, @psrc, @len);
  glCompileShader(handle);
  glGetShaderiv(handle, GL_COMPILE_STATUS, @i);
  if i = 0 then
  begin
    len := 2048;
    s := '';  // to avoid old FPC warning
    SetLength(s, len);
    //glGetShaderInfoLog(result, length(errorstr), @len, @errorstr[1]);   // gles header difference!
    glGetShaderInfoLog(handle, length(s), @len, @s[1]);
    glDeleteShader(handle);

    raise Exception.create('Error compiling '+ShaderTypeName[shadertype]+' shader: '+s);
  end;

  if shadertype = stVertex then FVSHandle := handle
                           else FFSHandle := handle;

  glAttachShader(FPrgHandle, handle);
end;

procedure TShaderProgram.LinkProgram;
var
  i : integer;
  len : TGLSizei;
  s : string;
begin
  glLinkProgram(FPrgHandle);
  glGetProgramiv(FPrgHandle, GL_LINK_STATUS, @i);
  if i = 0 then
  begin
    len := 2048;
    s := '';
    SetLength(s, len);  // to avoid old FPC warning

    //glGetShaderInfoLog(result, length(s), @len, @s[1]);    // gles header difference!
    glGetShaderInfoLog(FPrgHandle, length(s), @len, @s[1]);
    glDeleteProgram(FPrgHandle);
    FPrgHandle := 0;
    raise Exception.Create('Error linking shader program: ' + s);
  end;
end;

procedure TShaderProgram.Activate;
begin
  glUseProgram(FPrgHandle);
  activeshader := self;
end;

{ TSimpleShader }

constructor TSimpleShader.Create;
begin
  inherited;

  CompileShader(stVertex,
     'uniform mat3 u_MVPMatrix;' + #10
   + 'attribute vec2 a_Position;' + #10
   + 'void main()' + #10
   + '{' + #10
   + '  gl_Position = vec4(u_MVPMatrix * vec3(a_Position, 1.0), 1.0);' + #10
   + '}' + #10
  );

  CompileShader(stFragment, ''
   {$ifdef ANDROID} +'precision mediump float;' + #10  {$endif}
   + 'uniform vec4 u_Color;' + #10
   + 'void main()' + #10
   + '{' + #10
   + '  gl_FragColor = u_Color;' + #10
   + '}' + #10
  );

  glBindAttribLocation(FPrgHandle, attrloc_position, 'a_Position');

  LinkProgram;

  Fu_MVPMatrix := glGetUniformLocation(FPrgHandle, 'u_MVPMatrix');
  Fu_Color := glGetUniformLocation(FPrgHandle, 'u_Color');
end;

procedure TSimpleShader.SetMVPMatrix(const mat : TMatrix);
var
  matgl : TMatrixGL;
begin
  ddmat_to_gl(matgl, mat);
  glUniformMatrix3fv(Fu_MVPMatrix, 1, false, @matgl);   // gles header difference!
end;

procedure TSimpleShader.SetColor(r, g, b, a : single);
begin
  glUniform4f(Fu_Color, r, g, b, a);
end;

procedure TSimpleShader.SetColor(const glc: TColorGL);
begin
  SetColor(glc.r, glc.g, glc.b, glc.a);
end;

{ TPrimitive }

constructor TPrimitive.Create(amode : GLint; avertexcount : integer; vertdata : PVertex);
begin
  drawmode := amode;
  vertexcount := avertexcount;
  SetLength(vertices, vertexcount);
  if (vertexcount > 0) and (vertdata <> nil) then
  begin
    Move(vertdata^, vertices[0], vertexcount * sizeof(TVertex));
  end;
  vao := -1;
  vbo := -1;
  bufferok := false;
end;

destructor TPrimitive.Destroy;
begin
  if vao >= 0 then glDeleteVertexArrays(1, @vao);
  if vbo >= 0 then glDeleteBuffers(1, @vbo);
  SetLength(vertices, 0);

  inherited Destroy;
end;

procedure TPrimitive.Draw;
begin
  if not bufferok then UpdateBuffer();

  glBindVertexArray(vao);
  glDrawArrays(drawmode, 0, vertexcount); // * 2);
end;

procedure TPrimitive.UpdateBuffer;
begin
  if vao < 0 then
  begin
    glGenVertexArrays(1, @vao);
    glGenBuffers(1, @vbo);
  end;

  glBindVertexArray(vao);
  glBindBuffer(GL_ARRAY_BUFFER, vbo);
  glBufferData(GL_ARRAY_BUFFER, vertexcount * sizeof(TVertex), @vertices[0], GL_STATIC_DRAW);
  glEnableVertexAttribArray(attrloc_position);
  glVertexAttribPointer(attrloc_position, 2, GL_FLOAT, false, 0, nil);

  bufferok := true;
end;

{ TDrawable }

constructor TDrawable.Create(aparent : TDrawGroup);
begin
  parent := aparent;
  if parent <> nil then parent.AddChild(self)
                   else parent := nil;
  x := 0;
  y := 0;
  scalex := 1;
  scaley := 1;
  rotation := 0;
  alpha := 1;
  visible := true;
end;

destructor TDrawable.Destroy;
begin
  inherited Destroy;
end;

procedure TDrawable.CopyProperties(acopyfrom : TDrawable);
begin
  x := acopyfrom.x;
  y := acopyfrom.y;
  scalex := acopyfrom.scalex;
  scaley := acopyfrom.scaley;
  rotation := acopyfrom.rotation;
  alpha := acopyfrom.alpha;
  visible := acopyfrom.visible;
end;

procedure TDrawable.UpdateMatrix;
var
  cosfi, sinfi : TddFloat;
begin
  cosfi := cos(PI * rotation / 180);
  sinfi := sin(PI * rotation / 180);

  matrix[0] := scalex * cosfi;
  matrix[1] := - sinfi * scaley;

  matrix[2] := sinfi * scalex;
  matrix[3] := scaley * cosfi;

  matrix[4] := x;
  matrix[5] := y;
end;

{ TShape }

constructor TShape.Create(aparent : TDrawGroup);
begin
  inherited Create(aparent);

  // white is the default color
  color.r := 1;
  color.g := 1;
  color.b := 1;
  color.a := 1; // default alpha

  SetLength(primitives, 0);
end;

destructor TShape.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TShape.AddPrimitive(amode : GLint; avertcount : integer; avertdata : PVertex) : TPrimitive;
begin
  result := TPrimitive.Create(amode, avertcount, avertdata);

  SetLength(primitives, length(primitives) + 1);
  primitives[length(primitives) - 1] := result;
end;

procedure TShape.Clear;
var
  p : TPrimitive;
begin
  for p in primitives do p.Free;
  SetLength(primitives, 0);
end;

procedure TShape.SetColor(r, g, b : TddFloat);
begin
  color.r := r;
  color.b := b;
  color.g := g;
end;

procedure TShape.SetColor(r, g, b, a : TddFloat);
begin
  color.r := r;
  color.b := b;
  color.g := g;
  color.a := a;
end;

procedure TShape.Draw(const apmatrix : TMatrix; apalpha : TddFloat);
var
  rmatrix : TMatrix;
  ralpha  : TddFloat;
  dprim   : TPrimitive;
begin
  UpdateMatrix();  // calculates self.matrix from scale[xy], rotation, +[xy]

  // calculate rmatrix, ralpha
  ddmat_mul(rmatrix, self.matrix, apmatrix);
  ralpha := apalpha * self.alpha;

  // select the proper shader for shape drawing:
  fixcolorshader.Activate();

  // pass the matrix to the shader
  fixcolorshader.SetMVPMatrix(rmatrix);
  fixcolorshader.SetColor(color.r, color.g, color.b, color.a * ralpha);

  for dprim in primitives do
  begin
    dprim.Draw();
  end;
end;

{ TClonedShape }

constructor TClonedShape.Create(aparent : TDrawGroup; aoriginal : TShape);
begin
  inherited Create(aparent);

  original := aoriginal;
  if original <> nil then
  begin
    // copy the original color
    color := original.color;
    CopyProperties(original);
  end;
end;

destructor TClonedShape.Destroy;
begin
  inherited Destroy;
end;

procedure TClonedShape.SetColor(r, g, b : TddFloat);
begin
  color.r := r;
  color.b := b;
  color.g := g;
end;

procedure TClonedShape.SetColor(r, g, b, a : TddFloat);
begin
  color.r := r;
  color.b := b;
  color.g := g;
  color.a := a;
end;

procedure TClonedShape.Draw(const apmatrix : TMatrix; apalpha : TddFloat);
var
  rmatrix : TMatrix;
  ralpha  : TddFloat;
  dprim   : TPrimitive;
begin
  UpdateMatrix();  // calculates self.matrix from scale[xy], rotation, +[xy]

  // calculate rmatrix, ralpha
  ddmat_mul(rmatrix, self.matrix, apmatrix);
  ralpha := apalpha * self.alpha;

  // select the proper shader for shape drawing:
  fixcolorshader.Activate();

  // pass the matrix to the shader
  fixcolorshader.SetMVPMatrix(rmatrix);
  fixcolorshader.SetColor(color.r, color.g, color.b, color.a * ralpha);

  for dprim in original.primitives do
  begin
    dprim.Draw();
  end;
end;

{ TDrawGroup }

constructor TDrawGroup.Create(aparent : TDrawGroup);
begin
  SetLength(children, 0);

  inherited Create(aparent);
end;

destructor TDrawGroup.Destroy;
var
  dr : TDrawable;
begin
  for dr in children do dr.Free;
  SetLength(children, 0);
  inherited Destroy;
end;

procedure TDrawGroup.AddChild(adr : TDrawable);
begin
  SetLength(children, length(children) + 1);
  children[length(children) - 1] := adr;
  adr.parent := self;
end;

procedure TDrawGroup.Draw(const apmatrix : TMatrix; apalpha : TddFloat);
var
  rmatrix : TMatrix;
  ralpha  : TddFloat;
  drobj   : TDrawable;
begin
  UpdateMatrix();  // calculates self.matrix from scale[xy], rotation, +[xy]

  // calculate rmatrix, ralpha
  ddmat_mul(rmatrix, self.matrix, apmatrix);
  ralpha := apalpha * self.alpha;

  for drobj in children do
  begin
    if drobj.visible then
    begin
      drobj.Draw(rmatrix, ralpha);
    end;
  end;
end;

{ TClonedGroup }

constructor TClonedGroup.Create(aparent : TDrawGroup; aoriginal : TDrawGroup);
begin
  inherited Create(aparent);
  original := aoriginal;
  if original <> nil then
  begin
    CopyProperties(original);
  end;
end;

destructor TClonedGroup.Destroy;
begin
  inherited Destroy;
end;

procedure TClonedGroup.Draw(const apmatrix : TMatrix; apalpha : TddFloat);
var
  rmatrix : TMatrix;
  ralpha  : TddFloat;
  drobj   : TDrawable;
begin
  UpdateMatrix();  // calculates self.matrix from scale[xy], rotation, +[xy]

  // calculate rmatrix, ralpha
  ddmat_mul(rmatrix, self.matrix, apmatrix);
  ralpha := apalpha * self.alpha;

  for drobj in original.children do
  begin
    if drobj.visible then
    begin
      drobj.Draw(rmatrix, ralpha);
    end;
  end;
end;

{ TddScene }

constructor TddScene.Create(aowner : TComponent; aparent : TWinControl);
begin
  inherited Create(aowner);
  parent := aparent;
  name := 'ddScene';

  ddmat_identity(mat_projection);

  root := TDrawGroup.Create(nil);  // no parent here

  Align := alClient; // fill the client area

  OpenGLMajorVersion := 3;   // This is important in order to use OpenGL Context 3.3
  OpenGLMinorVersion := 3;

  bgcolor.r := 0;
  bgcolor.g := 0;
  bgcolor.b := 0;
  bgcolor.a := 1;

  InitOpenGL;

  MakeCurrent;

  ReadExtensions;
  ReadImplementationProperties;

  glEnable(GL_BLEND);                                // Alphablending an
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA); // Sortierung der Primitiven von hinten nach vorne.

  fixcolorshader := TSimpleShader.Create;
  fixcolorshader.Activate;

  tst := TddTester.Create;
end;

destructor TddScene.Destroy;
begin
  inherited Destroy;
end;

procedure TddScene.DoOnResize;
begin
  inherited DoOnResize;

  // do some resize the scene...
end;

procedure TddScene.SetViewPort(awidth, aheight : integer);
begin
  glViewport(0, 0, awidth, aheight);

  mat_projection[0] := 2 / awidth;
  mat_projection[1] := 0;

  mat_projection[2] := 0;
  mat_projection[3] := -2 / aheight;

  mat_projection[4] := -1;
  mat_projection[5] := 1;
end;

procedure TddScene.DoOnPaint;
begin
  inherited DoOnPaint;  // calls the OnPaint handler

  if not MakeCurrent() then
  begin
    Exit;  // do not go on on errors
  end;

  // 1. prepare the drawing

  SetViewPort(self.Width, self.Height); // calculates the projection matrix

  glClearColor(bgcolor.r, bgcolor.g, bgcolor.b, bgcolor.a);
  glClear(GL_COLOR_BUFFER_BIT);  // initialize the framebuffer with the background color

  // 2. do the drawing....
  root.Draw(mat_projection, 1);
  //tst.Draw;

  // 3. show results on the screen. May be synchronized to the refresh rate
  SwapBuffers;
end;

initialization

begin
  activeshader := nil;
  fixcolorshader := nil;
end;

end.

