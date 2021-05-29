local html = import 'html.libsonnet';
local jekyll = import 'jekyll.libsonnet';
local content = import 'stdlib-content.jsonnet';

local
  h1 = html.h1,
  p = html.p;

local manifestJsonSingleLine(val) = std.strReplace(std.manifestJsonEx(val, ''), '\n', ' ');

local exampleDoc(ex) =
  local exRep =
    if std.isString(ex) then
      ex
    else
      html.spaceless([html.code({}, ex.input), ' yields ', html.code({}, manifestJsonSingleLine(ex.output))])
  ;
  html.p({}, html.spaceless(['Example: ', exRep, '.']))
;

local hgroup(body) = html.div({ class: 'hgroup' }, body);
local hgroup_inline(body) = html.div({ class: 'hgroup-inline' }, [body, '<div style="clear: both"></div>']);
local panel(body) = html.div({ class: 'panel' }, body);

local in_panel(body) = hgroup(hgroup_inline(panel(body)));

local fieldParams(f) =
  if std.objectHas(f, 'params') then
    '(' + std.join(', ', f.params) + ')'
  else
    ''
;

local fieldDescription(f) =
  if std.isString(f.description) then
    html.p({}, f.description)
  else
    f.description
;

local fieldDoc(f, prefix) =
  [
    in_panel(html.h4({ id: f.name }, prefix + '.' + f.name + fieldParams(f))),
    in_panel([
      if std.objectHas(f, 'availableSince') then (
        html.p(
          {},
          html.em(
            {},
            if f.availableSince == 'upcoming' then
              'Available in upcoming release.'
            else
              'Available since version ' + f.availableSince + '.'
          )
        )
      ),
      fieldDescription(f),
      if std.objectHas(f, 'examples') then [
        exampleDoc(ex)
        for ex in f.examples
      ] else [],
    ]),
    '',
  ];

local group(group_spec, prefix) =
  [
    in_panel(html.h3({ id: group_spec.id }, group_spec.name)),
    if std.objectHas(group_spec, 'intro') then in_panel(group_spec.intro),
    '',
    [fieldDoc(f, prefix) for f in group_spec.fields],
    '',
  ];

local stdlibPage = [
  in_panel(html.h1({id: 'standard_library'}, 'Standard Library')),
  '',
  in_panel(content.intro),
  '',
  [group(g, content.prefix) for g in content.groups],
];

local stdlibFrontMatter = {
  layout: 'default',
  title: 'Standard Library',
};

jekyll.renderWithFrontMatter(stdlibFrontMatter, stdlibPage)
