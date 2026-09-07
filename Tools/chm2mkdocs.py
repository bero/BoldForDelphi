#!/usr/bin/env python3
"""Convert the original Bold for Delphi help (Help\\BfD.chm, Time2HELP/BoldSoft template)
into MkDocs reference pages.

    python Tools\\chm2mkdocs.py --units BoldSystem BoldHandles ...     # selected units
    python Tools\\chm2mkdocs.py --all                                  # every unit

Output: docs/reference/<Unit>/index.md and docs/reference/<Unit>/<TClass>.md, plus
docs/reference/index.md and a generated nav block in mkdocs.yml (between the markers
"# BEGIN generated reference nav" and "# END generated reference nav").

The help describes Bold 4.0 (2004). Classes that no longer exist in Source/ are marked;
members added since then are not in the help and therefore not on the pages.
Requires: beautifulsoup4, lxml; hh.exe (HTML Help, part of Windows) for decompiling.
"""
import argparse
import html
import os
import re
import shutil
import subprocess
import sys
from pathlib import Path

from bs4 import BeautifulSoup, NavigableString, Tag

REPO = Path(__file__).resolve().parents[1]
CHM = REPO / 'Help' / 'BfD.chm'
OUT = REPO / 'docs' / 'reference'
NAV_BEGIN = '# BEGIN generated reference nav'
NAV_END = '# END generated reference nav'


# ---------------------------------------------------------------- decompile
def decompile(chm: Path, target: Path) -> Path:
    if (target / 'BfD.hhc').exists():
        return target
    target.mkdir(parents=True, exist_ok=True)
    subprocess.run(['hh.exe', '-decompile', str(target), str(chm)], check=False)
    # hh.exe returns before it is done
    import time
    for _ in range(60):
        if (target / 'BfD.hhc').exists() and len(list(target.glob('IDH_*.htm'))) > 14000:
            break
        time.sleep(1)
    if not (target / 'BfD.hhc').exists():
        sys.exit('decompiling %s failed' % chm)
    return target


# ---------------------------------------------------------------- source check
def classes_in_source(source_dir: Path) -> set:
    names = set()
    # classes, exceptions (E...) and interfaces (I...) alike; forward declarations count too
    pat = re.compile(r'^\s*([A-Za-z_][A-Za-z0-9_]*)\s*=\s*(?:class|interface)\b', re.M)
    for pas in source_dir.rglob('*.pas'):
        if 'Deprecated' in pas.parts:
            continue
        try:
            text = pas.read_text(encoding='utf-8', errors='replace')
        except OSError:
            continue
        names.update(pat.findall(text))
    return names


# ---------------------------------------------------------------- html -> markdown
ICON_RE = re.compile(r'images/(readonly|public|protect|protected|private|published|override_small|virtual_small|'
                     r'abstract_small|dynamic_small|class_small|top_wide|bottom_wide|left_round|right_round|trans_high)\.gif$', re.I)


class Converter:
    def __init__(self, class_unit: dict, generated_classes: set):
        self.class_unit = class_unit          # TClass -> Unit (from the help)
        self.generated = generated_classes    # classes that get a page
        self.current_unit = ''
        self.images_used = set()              # non-icon images referenced by converted text

    # link targets
    def class_link(self, cls: str, text: str = None) -> str:
        text = text or cls
        if cls in self.generated:
            unit = self.class_unit.get(cls, '')
            rel = f'{cls}.md' if unit == self.current_unit else f'../{unit}/{cls}.md'
            return f'[{text}]({rel})'
        return f'`{text}`'

    def href_to_md(self, href: str, text: str) -> str:
        href = href.split('#')[0]
        m = re.match(r'IDH_Class_(\w+)\.htm', href)
        if m:
            return self.class_link(m.group(1), text)
        m = re.match(r'IDH_Unit_(\w+)\.htm', href)
        if m:
            unit = m.group(1)
            if any(self.class_unit.get(c) == unit for c in self.generated):
                rel = 'index.md' if unit == self.current_unit else f'../{unit}/index.md'
                return f'[{text}]({rel})'
            return f'`{text}`'
        m = re.match(r'IDH_(T\w+?)_(\w+)\.htm', href)
        if m and m.group(1) in self.class_unit:
            cls, member = m.group(1), m.group(2)
            if cls in self.generated:
                unit = self.class_unit.get(cls, '')
                rel = f'{cls}.md' if unit == self.current_unit else f'../{unit}/{cls}.md'
                return f'[{text}]({rel}#{member.lower()})'
            return f'`{text}`'
        return text  # other topics (concepts, dependencies) are not generated yet

    def inline(self, node) -> str:
        """Inline conversion of a node's children."""
        return ''.join(self.inline_node(child) for child in node.children)

    def inline_node(self, child) -> str:
        """Inline conversion of one node, the node itself included."""
        if isinstance(child, NavigableString):
            return self.inline_string(child)
        if not isinstance(child, Tag):
            return ''
        name = child.name.lower()
        if name == 'a' and child.get('href'):
            return self.href_to_md(child['href'], self.inline(child).strip())
        if name == 'font' and 'courier' in (child.get('face') or '').lower():
            code = self.inline(child).strip().replace('`', '')
            return f'`{code}`' if code else ''
        if name == 'b':
            inner = self.inline(child).strip()
            return f'**{inner}**' if inner else ''
        if name == 'i':
            inner = self.inline(child).strip()
            return f'*{inner}*' if inner else ''
        if name == 'br':
            return '  \n'
        if name == 'img':
            src = child.get('src', '')
            if not src or ICON_RE.match(src):
                return ''
            self.images_used.add(src)
            return f'![]({"../images/" + src.split("/")[-1]})'
        return self.inline(child)

    def block(self, node) -> str:
        """Block conversion: paragraphs, lists, tables, br."""
        parts = []
        buf = []

        def flush():
            text = ''.join(buf).strip()
            buf.clear()
            if text:
                parts.append(text)

        for child in node.children:
            if isinstance(child, NavigableString):
                buf.append(self.inline_string(child))
                continue
            name = child.name.lower()
            if name == 'p':
                flush()
                parts.append(self.block(child))
            elif name in ('ul', 'ol'):
                flush()
                items = []
                for li in child.find_all('li', recursive=False):
                    items.append('- ' + re.sub(r'\s*\n\s*', ' ', self.block(li)).strip())
                parts.append('\n'.join(items))
            elif name == 'table':
                flush()
                parts.append(self.table(child))
            elif name == 'br':
                buf.append('  \n')
            else:
                buf.append(self.inline_node(child) if name in ('a', 'font', 'b', 'i', 'img') else self.block(child))
        flush()
        text = '\n\n'.join(p for p in parts if p)
        text = re.sub(r'(  \n)+\s*\n', '\n\n', text)      # br before paragraph break
        text = re.sub(r'\n{3,}', '\n\n', text)
        return text.strip()

    def inline_string(self, s) -> str:
        s = str(s).replace('\r', '').replace('\xad', '')
        return re.sub(r'[ \t\n]+', ' ', s)

    def table(self, tbl) -> str:
        rows = []
        for tr in tbl.find_all('tr', recursive=False):
            cells = [re.sub(r'\s*\n\s*', ' ', self.block(td)).strip().replace('|', '\\|')
                     for td in tr.find_all(['td', 'th'], recursive=False)]
            if any(cells):
                rows.append(cells)
        if not rows:
            return ''
        width = max(len(r) for r in rows)
        rows = [r + [''] * (width - len(r)) for r in rows]
        head = '| ' + ' | '.join(rows[0]) + ' |'
        sep = '|' + '---|' * width
        body = ['| ' + ' | '.join(r) + ' |' for r in rows[1:]]
        return '\n'.join([head, sep] + body)


# ---------------------------------------------------------------- topic parsing
def soup_of(path: Path) -> BeautifulSoup:
    raw = path.read_bytes().decode('iso-8859-1')
    return BeautifulSoup(raw, 'lxml')


def content_cells(soup):
    return [td for td in soup.find_all('td', class_='content')]


LABEL_RE = re.compile(r'\*\*(Unit|Declaration|Hierarchy|Subclasses|Description|Applies to)\*\*')


def summary_and_body(cells, conv):
    """Time2HELP omits the summary table when the summary is empty, so a topic has either
    (summary, body) or just (body). The body is the cell that carries the bold labels."""
    if not cells:
        return '', ''
    first = conv.inline(cells[0]).strip()
    if len(cells) == 1:
        body = conv.block(cells[0])
        return ('', body) if LABEL_RE.search(body) else (first, '')
    return first, conv.block(cells[1])


PLACEHOLDER = re.compile(r'\*\*Advanced or internal topic\*\*.*?Send a request to get this topic published\.', re.S)


def split_sections(md: str) -> dict:
    """Split at the known top-level labels, which the template writes as '**Label**' followed
    by a line break, inline with the surrounding text."""
    labels = ['Unit', 'Declaration', 'Hierarchy', 'Subclasses', 'Description', 'Applies to']
    parts = re.split(r'\*\*(%s)\*\*\s*(?:  \n|\n)?' % '|'.join(labels), md)
    sections = {}
    for i in range(1, len(parts) - 1, 2):
        text = PLACEHOLDER.sub('', parts[i + 1]).strip()
        sections[parts[i]] = text
    return sections


def plain_code(md: str) -> str:
    """Declaration text: drop Markdown links, bold and code marks, unescape entities."""
    md = re.sub(r'\[([^\]]+)\]\([^)]*\)', r'\1', md)
    md = md.replace('**', '').replace('`', '').replace('  \n', ' ')
    return html.unescape(re.sub(r'\s+', ' ', md)).strip()


def parse_member_table(soup, anchor: str):
    """Rows of the 'Introduced <anchor>' table following <A Name=anchor>."""
    a = soup.find('a', attrs={'name': anchor})
    if not a:
        return []
    tbl = a.find_next('table', class_='list')
    rows = []
    if not tbl:
        return rows
    for tr in tbl.find_all('tr'):
        link = tr.find('a', href=True)
        if not link:
            continue
        cells = tr.find_all('td', class_='List')
        summary = cells[-1].get_text(' ', strip=True) if len(cells) > 1 else ''
        flags = []
        for img in tr.find_all('img'):
            src = (img.get('src') or '').lower()
            for key, flag in (('protect', 'protected'), ('private', 'private'), ('published', 'published'),
                              ('override', 'override'), ('virtual', 'virtual'), ('abstract', 'abstract'),
                              ('dynamic', 'dynamic'), ('readonly', 'read-only')):
                if key in src and flag not in flags:
                    flags.append(flag)
        rows.append((link.get_text(strip=True), link['href'], summary, ', '.join(flags)))
    return rows


def body_without_member_tables(td):
    """The class body cell continues with the 'Introduced ...' member tables after the
    description; drop everything from the first member anchor on."""
    anchor = td.find('a', attrs={'name': re.compile(r'^(Properties|Methods|Events)$')})
    if anchor is None:
        return td
    # climb to the child of td that contains the anchor, then remove it and all following siblings
    node = anchor
    while node.parent is not None and node.parent is not td:
        node = node.parent
    while node is not None:
        nxt = node.next_sibling
        node.extract()
        node = nxt
    return td


def parse_class(path: Path, conv: Converter) -> dict:
    soup = soup_of(path)
    properties = parse_member_table(soup, 'Properties')
    methods = parse_member_table(soup, 'Methods')
    events = parse_member_table(soup, 'Events')
    cells = content_cells(soup)
    if len(cells) > 1:
        body_without_member_tables(cells[1])
    elif cells:
        body_without_member_tables(cells[0])
    summary, body_md = summary_and_body(cells, conv)
    sections = split_sections(body_md)
    return {
        'name': re.sub(r'\s+Class$', '', soup.title.get_text(strip=True)) if soup.title else path.stem,
        'summary': summary,
        'unit': plain_code(sections.get('Unit', '')),
        'declaration': plain_code(sections.get('Declaration', '')),
        'hierarchy': sections.get('Hierarchy', ''),
        'subclasses': sections.get('Subclasses', ''),
        'description': sections.get('Description', ''),
        'properties': properties,
        'methods': methods,
        'events': events,
    }


def parse_member(path: Path, conv: Converter) -> dict:
    soup = soup_of(path)
    summary, body_md = summary_and_body(content_cells(soup), conv)
    sections = split_sections(body_md)
    return {
        'summary': summary,
        'declaration': plain_code(sections.get('Declaration', '')),
        'description': sections.get('Description', ''),
    }


def parse_unit(path: Path, conv: Converter) -> dict:
    soup = soup_of(path)
    cells = content_cells(soup)
    summary = conv.inline(cells[0]).strip() if cells else ''
    description = conv.block(cells[1]) if len(cells) > 1 else ''
    classes = []
    grey = soup.find('td', class_='grey', string=re.compile(r'^\s*Classes\s*$'))
    if grey:
        tbl = grey.find_next('table', class_='list')
        if tbl:
            for a in tbl.find_all('a', href=re.compile(r'^IDH_Class_')):
                classes.append(a.get_text(strip=True))
    return {'summary': summary, 'description': description, 'classes': classes}


# ---------------------------------------------------------------- page rendering
def hierarchy_md(hier: str, conv: Converter) -> str:
    # "TComponent  \n ↑  \n [X](..)  \n ↑ ..." -> ordered list root first
    hier = html.unescape(hier).replace('  \n', '\n')
    items = [x.strip() for x in re.split(r'\s*↑\s*|\n', hier) if x.strip() and x.strip() != '↑']
    return '\n'.join(f'{i}. {x}' for i, x in enumerate(items, 1))


def render_class(cls: dict, members: dict, in_source: bool, unit: str) -> str:
    out = [f'# {cls["name"]}', '']
    if cls['summary']:
        out += [cls['summary'], '']
    if not in_source:
        out += ['!!! warning "Not in the current source"',
                '    This class is documented in the Bold 4.0 help, but no class or interface with this name',
                '    is declared in `Source/` today. It was removed or reshaped (for example into a record).', '']
    out += [f'**Unit**: [{unit}](index.md)', '']
    if cls['declaration']:
        out += ['## Declaration', '', '```delphi', html.unescape(cls['declaration']), '```', '']
    if cls['hierarchy']:
        out += ['## Hierarchy', '', hierarchy_md(cls['hierarchy'], None), '']
    if cls['subclasses'] and cls['subclasses'].strip().lower() != 'none':
        out += ['## Subclasses', '', cls['subclasses'], '']
    if cls['description']:
        out += ['## Description', '', cls['description'], '']
    for kind in ('properties', 'methods', 'events'):
        rows = cls[kind]
        if not rows:
            continue
        out += [f'## {kind.capitalize()}', '', '| Name | Summary | Notes |', '|---|---|---|']
        for name, href, summary, flags in rows:
            out.append(f'| [{name}](#{name.lower()}) | {summary.replace("|", "/")} | {flags} |')
        out.append('')
        for name, href, summary, flags in rows:
            m = members.get(href)
            out += [f'### {name}', '']
            if m and m['declaration']:
                out += ['```delphi', html.unescape(m['declaration']), '```', '']
            text = (m or {}).get('description') or ''
            if not text:
                text = (m or {}).get('summary') or summary
            if text and text != summary_marker(cls, name):
                out += [text, '']
    out += ['---', '', '*Generated from the Bold 4.0 help (`Help/BfD.chm`) by `Tools/chm2mkdocs.py`; '
            'member lists reflect Bold 4.0, see the source for members added since.*', '']
    return '\n'.join(out)


def summary_marker(cls, name):
    return None


def render_unit_index(unit: str, info: dict, classes: list) -> str:
    out = [f'# {unit}', '']
    if info.get('summary'):
        out += [info['summary'], '']
    if info.get('description'):
        out += [info['description'], '']
    out += ['## Classes', '', '| Class | Summary |', '|---|---|']
    for cls in classes:
        out.append(f'| [{cls["name"]}]({cls["name"]}.md) | {cls["summary"].replace("|", "/")} |')
    out.append('')
    return '\n'.join(out)


def write(path: Path, text: str):
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(text.replace('\r\n', '\n').replace('\n', '\r\n'), encoding='utf-8-sig')


def update_nav(mkdocs_yml: Path, units: list, unit_classes: dict):
    text = mkdocs_yml.read_text(encoding='utf-8')
    nl = '\r\n' if '\r\n' in text else '\n'
    lines = ['  - Reference:', '    - reference/index.md']
    for unit in units:
        lines.append(f'    - {unit}:')
        lines.append(f'      - Overview: reference/{unit}/index.md')
        for cls in unit_classes[unit]:
            lines.append(f'      - {cls["name"]}: reference/{unit}/{cls["name"]}.md')
    block = f'{NAV_BEGIN}{nl}' + nl.join(lines) + f'{nl}{NAV_END}'
    if NAV_BEGIN in text:
        text = re.sub(re.escape(NAV_BEGIN) + r'.*?' + re.escape(NAV_END), lambda m: block, text, flags=re.S)
    else:
        # append to the nav: right after the last top-level nav entry (before the next top-level key)
        m = re.search(r'^nav:\s*$', text, re.M)
        if not m:
            sys.exit('mkdocs.yml has no nav section')
        after = text[m.end():]
        m2 = re.search(r'^\S', after, re.M)  # next top-level key
        insert_at = m.end() + (m2.start() if m2 else len(after))
        text = text[:insert_at].rstrip(nl) + nl + block + nl + nl + text[insert_at:]
    mkdocs_yml.write_text(text, encoding='utf-8')


# ---------------------------------------------------------------- main
def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('--chm-dir', default=str(REPO / '.chm-decompiled'), help='folder for the decompiled help')
    ap.add_argument('--units', nargs='*', default=[], help='unit names to convert')
    ap.add_argument('--all', action='store_true')
    ap.add_argument('--no-nav', action='store_true')
    args = ap.parse_args()

    chm_dir = decompile(CHM, Path(args.chm_dir))
    conv = Converter({}, set())

    # units and the class -> unit map from every unit page
    unit_info, class_unit = {}, {}
    for up in sorted(chm_dir.glob('IDH_Unit_*.htm')):
        unit = up.stem[len('IDH_Unit_'):]
        info = parse_unit(up, conv)
        unit_info[unit] = info
        for c in info['classes']:
            class_unit[c] = unit
    units = sorted(unit_info) if args.all else args.units
    missing = [u for u in units if u not in unit_info]
    if missing:
        sys.exit('unknown units: ' + ', '.join(missing))

    generated, owner = set(), {}
    for u in units:
        for c in unit_info[u]['classes']:
            if c not in owner and (chm_dir / f'IDH_Class_{c}.htm').exists():
                owner[c] = u
                generated.add(c)
    class_unit.update(owner)               # links to generated classes point where the page is written
    conv = Converter(class_unit, generated)
    present = classes_in_source(REPO / 'Source')

    unit_classes = {}
    n_classes = n_members = n_missing = 0
    for unit in units:
        conv.current_unit = unit
        classes = []
        for cname in unit_info[unit]['classes']:
            cpath = chm_dir / f'IDH_Class_{cname}.htm'
            if not cpath.exists() or owner.get(cname) != unit:
                continue
            cls = parse_class(cpath, conv)
            members = {}
            for kind in ('properties', 'methods', 'events'):
                for name, href, summary, flags in cls[kind]:
                    mpath = chm_dir / href
                    if mpath.exists():
                        members[href] = parse_member(mpath, conv)
                        n_members += 1
            in_source = cls['name'] in present
            if not in_source:
                n_missing += 1
            write(OUT / unit / f'{cls["name"]}.md', render_class(cls, members, in_source, unit))
            classes.append(cls)
            n_classes += 1
        # re-render the unit index with the unit's own link context
        write(OUT / unit / 'index.md', render_unit_index(unit, unit_info[unit], classes))
        unit_classes[unit] = classes

    # top index
    conv.current_unit = ''
    out = ['# Class Reference', '',
           'Generated from the original Bold for Delphi help (Bold 4.0, `Help/BfD.chm`). Descriptions are '
           'the Boldsoft texts; classes that have since been removed are marked, members added since 2004 are '
           'not listed. The hand-written [class guides](../classes/index.md) cover usage patterns and examples.', '',
           '| Unit | Summary | Classes |', '|---|---|---|']
    for unit in units:
        out.append(f'| [{unit}]({unit}/index.md) | {unit_info[unit]["summary"].replace("|", "/")} | {len(unit_classes[unit])} |')
    write(OUT / 'index.md', '\n'.join(out) + '\n')

    if conv.images_used:
        (OUT / 'images').mkdir(parents=True, exist_ok=True)
        for src in conv.images_used:
            f = chm_dir / src
            if f.exists():
                shutil.copy2(f, OUT / 'images' / f.name)
        print(f'images copied: {len(conv.images_used)}')

    if not args.no_nav:
        update_nav(REPO / 'mkdocs.yml', units, unit_classes)
    print(f'units: {len(units)}  classes: {n_classes} ({n_missing} not in current source)  members: {n_members}')


if __name__ == '__main__':
    main()
