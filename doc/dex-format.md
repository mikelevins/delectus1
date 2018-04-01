# Dex file format

Dex uses SQLite files to store all data, one document per file. This
document describes how those files are organized.

## Dex UI

Dex supports the concept of a document called a **deck**. A deck is
made up of **cards**.

Dex presents a deck in a window. The contents of the deck appear in
the window as **widgets**.

Supported widgets include:

- Graphics
- Text fields
- Buttons
- Lists

Dex users can choose these widgets from a palette and add them to a
window. They can then edit the widgets to give them data and
behavior. The data can be graphics, text, numbers, or lists. The
behavior is described by **scripts** attached to widgets.

Dex stores all of the data needed to present a deck in a SQlite
file. This document describes how each Dex concept is represented in a
SQLite file.

## Elements of a Deck

A Dex file is a SQLite file with contents that represent all the
information needed to construct a Dex window. The file contains the
following types of elements:

- **Dex metadata:** Dex uses this information to confirm that the file
  is a valid and supported Dex file. 

- **Graphics:** A list of all images used anywhere in the deck

- **Fields:** A list of all text fields used anywhere in the deck

- **Buttons:** A list of all buttons used anywhere in the deck

- **Lists:** A list of all lists used anywhere in the deck

- **Scripts:** A list of all scripts used anywhere in the deck

- **Cards:** A list of all cards used anywhere in the deck

- **Backgrounds:** A list of all backgrounds used anywhere in the deck

- **Deck:** A description of the deck itself, including information
    like the ID of the first card in the deck and the deck's script.

## Presenting a Deck

When Dex is asked to open a deck, it reads the selected file, verifies
that it is a valid Deck, and then constructs a window to present its
contents. It follows these steps, not necessarily in this order:

1. Check that the file is valid

2. Read and execute the deck's init script, if any.

3. Determine the dimensions of the deck and create a window of the appropriate size

4. Determine which card to display first. Fetch the card from the
Cards table and get from it the background and widgets to use.

5. Instantiate the background and widgets.

6. Execute the background and card scripts, if any.

## How Dex Uses the SQLite File

Each element of a deck's presentation is represented as an entry in a
SQLite table, except for lists. Each list is a separate table.

Dex presents each card by instantiating a background object and one or
more widget objects that appear on top of the background. For each
deck, background, and card, there is an entry in the appropriate table
describing how to instantiate and initialize it.

Dex starts by determining the initial card; this is a value in the
metadata table. It looks up the card in the Cards table and collects
from it the background and widgets. It then looks up the background in
the Backgrounds table, and it looks up each widget in the appropriate
widget table.

Widgets can be placed on either the card or the background. A widget
placed on the background appears on every card that uses the same
background. A widget placed on the card disappears when you go to the
next card, even if the background is the same.

The background record lists all of the widgets that appear in the
background, together with their placement on the background and their
stacking order. The background record also stores an optional script
for the background.

Like a background record, a card record lists all the widgets in the
card and their placements, and stores a card script.

Each element of a deck is identified by an ID that is unqiue in the
deck. A deck uses an ID to identify the initial card. A card uses an
ID to identify its background. A background or a card uses IDs to
identify each widget that appears in it.

Lists are special. A list is represented by a SQLite table. Because a
card or background is a row in the Cards or Backgrounds table, and
because SQLite does not support storing a table in a table, each List
is represented by a separate table. When a user creates a List widget
and adds it to a card or background, a new table is added to the
SQLite file for the deck. The name of the new table is the ID of the
list widget.

## Scripts

Dex has a simple scripting language that users can use to customize
the behavior of decks, cards, backgrounds, and widgets. Each script is
stored in the Scripts table. Each deck, card, background, or widget
has a script associated with it. Each script is initially empty. When
a user edits the script of some element of a deck, the resulting
script is stored in the Scripts table. The object to which the script
is attached then stores the ID of the script in its script field.

