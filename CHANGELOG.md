# Revision history for taskwarrior

## 0.6.0.0

* Drop Wait constructor from Status. This is a breaking change and is only compatible with taskwarrior > 2.6.0

## 0.4.1.0

* Make JSON parsing compatible with taskwarrior 2.6.

## 0.4.0.0

### Breaking Change
* Change UDA Type from HashMap to ordered Map.

### Maintenance
* Update to aeson 2.0.
* Improve test.nix

## 0.3.0.0

* Only export `id` and `urgency` when non default.
* Change use of `[]` to `Set` because that matches the semantic better.

## 0.2.1.0

* Add helpers for onAdd and onModify hooks.
* Fix a bug where JSON without an `id` ar `urgency` key would be wrongly rejected.

## 0.2.0.0

### Breaking changes

* data Status: Removed RecurringChild, renamed RecurringParent to Recurring.

### Other API changes

* RecurringChild is now a new Type and a Maybe field of Task.
* Added the fields 'id' and 'urgency' to Task. They were regarded as UDAs before.
* Now exporting the data consructors of Priority
* Added To- and FromJSON for Status
* Add FromJSON for Priority and improved Parser errors for it

### Documentation

* Added documentation about adherence to the specification
* Improved linking in the docs
* Expanded README regarding help and contributions

### Development

* Github actions will now check formatting and linking in haddocks

### Thanks

Thanks to @ryantrinkle and @tom-audm for their contributions, which triggered this release.

## 0.1.2.4

* Fix a bug where recurring masks where parsed wrong
* Further (small) documentation improvementes

## 0.1.2.3

* Documentation improvements
* Loosen some dependency bounds

## 0.1.2.2

* Add `StrictData` pragma for better performance.

## 0.1.2.1

* Remove string-interpolate dependency

## 0.1.2.0 -- 2019-12-23

* Added `getUUIDs` function.

## 0.1.1.0 -- 2019-12-06

* Added `makeTask` and `createTask` functions.

## 0.1.0.0 -- 2019-11-08

* First version. Includes the data types, `saveTasks` and `getTasks`.
