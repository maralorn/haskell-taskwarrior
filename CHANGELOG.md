# Revision history for taskwarrior

## 0.2.0.0

### Most likely breaking changes

* data Status: Removed RecurringChild, renamed RecurringParent to Recurring. RecurringChild is now a Maybe field of Task.

### Possibly breaking changes

* Added the fields 'id' and 'urgency' to Task. They were regarded as UDAs before.

### Other API changes

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

Thanks to @ryantrinkle and @tom-audm for their contributions.

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
