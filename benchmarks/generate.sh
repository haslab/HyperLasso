#!/bin/bash

echo "Generating CMS"

python3 -m CMS.cms_any 1 1 > CMS/cms_any_1x1.smv
python3 -m CMS.cms_any 2 2 > CMS/cms_any_2x2.smv
python3 -m CMS.cms_any 3 3 > CMS/cms_any_3x3.smv

python3 -m CMS.cms_ndet 1 1 > CMS/cms_ndet_1x1.smv
python3 -m CMS.cms_ndet 2 2 > CMS/cms_ndet_2x2.smv
python3 -m CMS.cms_ndet 3 3 > CMS/cms_ndet_3x3.smv

python3 -m CMS.cms_max 1 1 > CMS/cms_max_1x1.smv
python3 -m CMS.cms_max 2 2 > CMS/cms_max_2x2.smv
python3 -m CMS.cms_max 3 3 > CMS/cms_max_3x3.smv

python3 -m CMS.gni 1 1 hp > CMS/gni_1x1.hp
python3 -m CMS.gni 2 2 hp > CMS/gni_2x2.hp
python3 -m CMS.gni 3 3 hp > CMS/gni_3x3.hp
python3 -m CMS.gni 1 1 ah > CMS/gni_1x1.ah
python3 -m CMS.gni 2 2 ah > CMS/gni_2x2.ah
python3 -m CMS.gni 3 3 ah > CMS/gni_3x3.ah

echo "Generating Isolation"

python3 -m Isolation.rc 2 1 > Isolation/rc_2x1.smv
python3 -m Isolation.rc 3 2 > Isolation/rc_3x2.smv
python3 -m Isolation.rc 4 3 > Isolation/rc_4x3.smv

python3 -m Isolation.ser 2 1 > Isolation/ser_2x1.smv
python3 -m Isolation.ser 3 2 > Isolation/ser_3x2.smv
python3 -m Isolation.ser 4 3 > Isolation/ser_4x3.smv

python3 -m Isolation.implies 2 1 hp > Isolation/implies_2x1.hp
python3 -m Isolation.implies 3 2 hp > Isolation/implies_3x2.hp
python3 -m Isolation.implies 4 3 hp > Isolation/implies_4x3.hp
python3 -m Isolation.implies 2 1 ah > Isolation/implies_2x1.ah
python3 -m Isolation.implies 3 2 ah > Isolation/implies_3x2.ah
python3 -m Isolation.implies 4 3 ah > Isolation/implies_4x3.ah

echo "Generating CNI"

python3 -m CNI.model 1 any > CNI/cni_any_1.smv
python3 -m CNI.model 2 any > CNI/cni_any_2.smv
python3 -m CNI.model 3 any > CNI/cni_any_3.smv
python3 -m CNI.model 4 any > CNI/cni_any_4.smv

python3 -m CNI.model 1 lte > CNI/cni_lte_1.smv
python3 -m CNI.model 2 lte > CNI/cni_lte_2.smv
python3 -m CNI.model 3 lte > CNI/cni_lte_3.smv
python3 -m CNI.model 4 lte > CNI/cni_lte_4.smv

python3 -m CNI.model_assigns 1 lte > CNI/cni_lte_assigns_1.smv
python3 -m CNI.model_assigns 2 lte > CNI/cni_lte_assigns_2.smv
python3 -m CNI.model_assigns 3 lte > CNI/cni_lte_assigns_3.smv
python3 -m CNI.model_assigns 4 lte > CNI/cni_lte_assigns_4.smv

python3 -m CNI.spec 1 hp > CNI/cni_1.hp
python3 -m CNI.spec 2 hp > CNI/cni_2.hp
python3 -m CNI.spec 3 hp > CNI/cni_3.hp
python3 -m CNI.spec 4 hp > CNI/cni_4.hp
python3 -m CNI.spec 1 ah > CNI/cni_1.ah
python3 -m CNI.spec 2 ah > CNI/cni_2.ah
python3 -m CNI.spec 3 ah > CNI/cni_3.ah
python3 -m CNI.spec 4 ah > CNI/cni_4.ah

python3 -m CNI.spec_assigns 1 ah > CNI/cni_assigns_1.ah
python3 -m CNI.spec_assigns 2 ah > CNI/cni_assigns_2.ah
python3 -m CNI.spec_assigns 3 ah > CNI/cni_assigns_3.ah
python3 -m CNI.spec_assigns 4 ah > CNI/cni_assigns_4.ah

echo "Generating SelfStabilization"

python3 -m SelfStabilization.herman 3 > SelfStabilization/herman_3.smv
python3 -m SelfStabilization.herman 5 > SelfStabilization/herman_5.smv
python3 -m SelfStabilization.herman 7 > SelfStabilization/herman_7.smv

python3 -m SelfStabilization.herman_assigns 3 > SelfStabilization/herman_assigns_3.smv
python3 -m SelfStabilization.herman_assigns 5 > SelfStabilization/herman_assigns_5.smv
python3 -m SelfStabilization.herman_assigns 7 > SelfStabilization/herman_assigns_7.smv

python3 -m SelfStabilization.selfstab_uf 3 hp > SelfStabilization/selfstab_uf_3.hp
python3 -m SelfStabilization.selfstab_uf 5 hp > SelfStabilization/selfstab_uf_5.hp
python3 -m SelfStabilization.selfstab_uf 7 hp > SelfStabilization/selfstab_uf_7.hp

python3 -m SelfStabilization.selfstab_uf_assigns 3 ah > SelfStabilization/selfstab_uf_assigns_3.ah
python3 -m SelfStabilization.selfstab_uf_assigns 5 ah > SelfStabilization/selfstab_uf_assigns_5.ah
python3 -m SelfStabilization.selfstab_uf_assigns 7 ah > SelfStabilization/selfstab_uf_assigns_7.ah

python3 -m SelfStabilization.selfstab_gf 3 hp > SelfStabilization/selfstab_gf_3.hp
python3 -m SelfStabilization.selfstab_gf 5 hp > SelfStabilization/selfstab_gf_5.hp
python3 -m SelfStabilization.selfstab_gf 7 hp > SelfStabilization/selfstab_gf_7.hp

python3 -m SelfStabilization.selfstab_gf_assigns 3 ah > SelfStabilization/selfstab_gf_assigns_3.ah
python3 -m SelfStabilization.selfstab_gf_assigns 5 ah > SelfStabilization/selfstab_gf_assigns_5.ah
python3 -m SelfStabilization.selfstab_gf_assigns 7 ah > SelfStabilization/selfstab_gf_assigns_7.ah

python3 -m SelfStabilization.selfstab_lf 3 hp > SelfStabilization/selfstab_lf_3.hp
python3 -m SelfStabilization.selfstab_lf 5 hp > SelfStabilization/selfstab_lf_5.hp
python3 -m SelfStabilization.selfstab_lf 7 hp > SelfStabilization/selfstab_lf_7.hp

python3 -m SelfStabilization.selfstab_lf_assigns 3 ah > SelfStabilization/selfstab_lf_assigns_3.ah
python3 -m SelfStabilization.selfstab_lf_assigns 5 ah > SelfStabilization/selfstab_lf_assigns_5.ah
python3 -m SelfStabilization.selfstab_lf_assigns 7 ah > SelfStabilization/selfstab_lf_assigns_7.ah

echo "Generating Bakery"

python3 -m Bakery.bakery 1 > Bakery/bakery_1.smv
python3 -m Bakery.bakery 2 > Bakery/bakery_2.smv
python3 -m Bakery.bakery 3 > Bakery/bakery_3.smv

python3 -m Bakery.bakery_assigns 1 > Bakery/bakery_assigns_1.smv
python3 -m Bakery.bakery_assigns 2 > Bakery/bakery_assigns_2.smv
python3 -m Bakery.bakery_assigns 3 > Bakery/bakery_assigns_3.smv

python3 -m Bakery.robust_crit 1 ah > Bakery/robust_crit_1.ah
python3 -m Bakery.robust_crit 1 hp > Bakery/robust_crit_1.hp
python3 -m Bakery.robust_crit 2 ah > Bakery/robust_crit_2.ah
python3 -m Bakery.robust_crit 2 hp > Bakery/robust_crit_2.hp
python3 -m Bakery.robust_crit 3 ah > Bakery/robust_crit_3.ah
python3 -m Bakery.robust_crit 3 hp > Bakery/robust_crit_3.hp

python3 -m Bakery.robust_ncrit 1 ah > Bakery/robust_ncrit_1.ah
python3 -m Bakery.robust_ncrit 1 hp > Bakery/robust_ncrit_1.hp
python3 -m Bakery.robust_ncrit 2 ah > Bakery/robust_ncrit_2.ah
python3 -m Bakery.robust_ncrit 2 hp > Bakery/robust_ncrit_2.hp
python3 -m Bakery.robust_ncrit 3 ah > Bakery/robust_ncrit_3.ah
python3 -m Bakery.robust_ncrit 3 hp > Bakery/robust_ncrit_3.hp

echo "Generating Controller"

python3 -m Controller.controller 1 1 > Controller/controller_1x1.smv
python3 -m Controller.controller 1 2 > Controller/controller_1x2.smv
python3 -m Controller.controller 2 0 > Controller/controller_2x0.smv
python3 -m Controller.controller 4 0 > Controller/controller_4x0.smv
python3 -m Controller.controller 4 2 > Controller/controller_4x2.smv
python3 -m Controller.controller 4 4 > Controller/controller_4x4.smv

python3 -m Controller.system 1 > Controller/system_1.smv
python3 -m Controller.system 2 > Controller/system_2.smv
python3 -m Controller.system 4 > Controller/system_4.smv

python3 -m Controller.spec1 2 0 ah > Controller/spec1_2x0.ah
python3 -m Controller.spec1 2 0 hp > Controller/spec1_2x0.hp
python3 -m Controller.spec1 4 0 ah > Controller/spec1_4x0.ah
python3 -m Controller.spec1 4 0 hp > Controller/spec1_4x0.hp
python3 -m Controller.spec1 4 2 ah > Controller/spec1_4x2.ah
python3 -m Controller.spec1 4 2 hp > Controller/spec1_4x2.hp

python3 -m Controller.spec2 1 1 ah > Controller/spec2_1x1.ah
python3 -m Controller.spec2 1 1 hp > Controller/spec2_1x1.hp
python3 -m Controller.spec2 1 2 ah > Controller/spec2_1x2.ah
python3 -m Controller.spec2 1 2 hp > Controller/spec2_1x2.hp
python3 -m Controller.spec2 4 4 ah > Controller/spec2_4x4.ah
python3 -m Controller.spec2 4 4 hp > Controller/spec2_4x4.hp

echo "Generating Planning"

python3 -m Planning.robot 3 > Planning/robot_3.smv
python3 -m Planning.robot 4 > Planning/robot_4.smv

python3 -m Planning.plan 3 1 ah > Planning/plan_3x1.ah
python3 -m Planning.plan 3 1 hp > Planning/plan_3x1.hp
python3 -m Planning.plan 3 2 ah > Planning/plan_3x2.ah
python3 -m Planning.plan 3 2 hp > Planning/plan_3x2.hp
python3 -m Planning.plan 3 3 ah > Planning/plan_3x3.ah
python3 -m Planning.plan 3 3 hp > Planning/plan_3x3.hp

python3 -m Planning.plan 4 1 ah > Planning/plan_4x1.ah
python3 -m Planning.plan 4 1 hp > Planning/plan_4x1.hp
python3 -m Planning.plan 4 2 ah > Planning/plan_4x2.ah
python3 -m Planning.plan 4 2 hp > Planning/plan_4x2.hp
python3 -m Planning.plan 4 3 ah > Planning/plan_4x3.ah
python3 -m Planning.plan 4 3 hp > Planning/plan_4x3.hp
python3 -m Planning.plan 4 4 ah > Planning/plan_4x4.ah
python3 -m Planning.plan 4 4 hp > Planning/plan_4x4.hp