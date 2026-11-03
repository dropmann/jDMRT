'use strict';

// An addon to jamovi's ANOVA: the terms offered are the ANOVA's model terms,
// which an addon sees (read only) as ui.parent_modelTerms.
module.exports = {

    duncanSupplier_updated: function(ui) {
        this.offerTerms(ui);
    },

    parent_modelTerms_changed: function(ui) {
        this.offerTerms(ui);
    },

    // a term taken out of the model is taken out of Duncan's terms too
    duncanSupplier_changed: function(ui) {
        let values = utils.itemsToValues(ui.duncanSupplier.value());
        utils.checkValue(ui.duncanTerms, true, values, FormatDef.term);
    },

    offerTerms: function(ui) {
        let termsList = utils.clone(ui.parent_modelTerms.value(), []);
        ui.duncanSupplier.setValue(utils.valuesToItems(termsList, FormatDef.term));
    }

};
