```mermaid
%%{init: {'theme': 'base', 'themeVariables': { 'fontFamily': 'Arial, Helvetica, sans-serif' }, 'flowchart': {'useWidth': true, 'width': 700}}}%%

%%
PLOS ONE JOURNAL SUBMISSION NOTES:
Font Family: Arial / Helvetica (Mandated by Journal)
Target Figure Width: 8.3 cm to 17.3 cm (3.27 to 6.81 inches)
Target Figure Height: Max 23.3 cm (9.17 inches)
Export Workflow: Export from mermaid.live as an SVG to maintain infinite scalability.
Convert SVG to TIFF or EPS at a minimum of 300 DPI matching the column width boundaries above before final submission.
%%

flowchart LR

subgraph IP["Inputs Pipeline"]
direction LR
A(["User created 'categories' Excel file"])
B(["Zotero library or collection"])
C(["CSV file exported from Zotero"])
E(["Citation RIS file"])
end

subgraph CE["Lit-tag Core Ecosystem"]
direction LR
D(["Lit-tag builder module"])
G(["Lit-tag CSV database"])
F(["Google Drive for sharing"])
H(["Lit-tag viewer module"])
end

subgraph UO["User Outputs"]
direction LR
I(["Searching and filtering"])
J(["Summary tables (CSV)"])
K(["Summary plots"])
L(["Reports (HTML, PDF, Word)"])
end

A -->|Creating lit-tag DB<br>Applying categories| D
A -->|Applying categories| H
B -->|Exports to| C
C -->|Creating lit-tag DB<br>Updating from Zotero| D
D -->|Generates| E
D <--> |Save edits<br>Update / Load DB| G
G <--> |Syncs with| F
G -->|Feeds data into| H
H -->|Powers| I
I -->|Generates| J
I -->|Generates| K
I -->|Generates| L

classDef inputStyle fill:#E6F4EA,stroke:#137333,stroke-width:1.5px,color:#137333,rx:4px,ry:4px;
classDef appStyle fill:#4F46E5,stroke:#3730A3,stroke-width:2px,color:#FFFFFF,font-weight:bold,rx:8px,ry:8px;
classDef storageStyle fill:#FFF7ED,stroke:#EA580C,stroke-width:1.5px,color:#C2410C;
classDef outputStyle fill:#E0F2FE,stroke:#0284C7,stroke-width:1.5px,color:#0369A1,rx:4px,ry:4px;

class A,B,C,E inputStyle;
class D,H appStyle;
class F,G storageStyle;
class I,J,K,L outputStyle;

linkStyle default stroke:#64748B,stroke-width:2px,interpolate:basis;
```
